;;; gptel-org.el --- Org functions for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'org-element)
(require 'outline)
(require 'subr-x)
(require 'mailcap)                    ;FIXME Avoid this somehow
(eval-when-compile (require 'gptel-request))

;; Functions used for saving/restoring gptel state in Org buffers
(defvar gptel--num-messages-to-send)
(defvar org-entry-property-inherited-from)
(defvar gptel-backend)
(defvar gptel--known-backends)
(defvar gptel-system-prompt)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel--link-type-cache)
(defvar gptel--preset)
(defvar so-long-action)                 ;so-long.el, Emacs 27+

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function gptel--model-capable-p "gptel-request")
(declare-function gptel--model-mime-capable-p "gptel-request")
(declare-function gptel--model-name "gptel-request")
(declare-function gptel--to-string "gptel-request")
(declare-function gptel--to-number "gptel-request")
(declare-function gptel--intern "gptel-request")
(declare-function gptel-backend-name "gptel-request" nil t)
(declare-function gptel--parse-buffer "gptel-request")
(declare-function gptel--parse-directive "gptel-request")
(declare-function gptel--with-buffer-copy "gptel-request" nil t)
(declare-function gptel--file-binary-p "gptel-request")
(declare-function gptel--get-buffer-bounds "gptel")
(declare-function gptel--valid-bounds "gptel")
(declare-function gptel--restore-props "gptel")
(declare-function gptel-mode "gptel")
(declare-function so-long "so-long" (&optional action))
(declare-function so-long-revert "so-long" ())
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs" nil t)
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")
(declare-function org-at-heading-p "org")

;; Bundle `org-element-lineage-map' if it's not available (for Org 9.67 or older)
(eval-and-compile
  (if (fboundp 'org-element-lineage-map)
      (progn (declare-function org-element-lineage-map "org-element-ast")
             (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))
    (defun gptel-org--element-lineage-map (datum fun &optional types with-self first-match)
      "Map FUN across ancestors of DATUM, from closest to furthest.

DATUM is an object or element.  For TYPES, WITH-SELF and
FIRST-MATCH see `org-element-lineage-map'.

This function is provided for compatibility with older versions
of Org."
      (declare (indent 2))
      (setq fun (if (functionp fun) fun `(lambda (node) ,fun)))
      (let ((up (if with-self datum (org-element-parent datum)))
	    acc rtn)
        (catch :--first-match
          (while up
            (when (or (not types) (org-element-type-p up types))
              (setq rtn (funcall fun up))
              (if (and first-match rtn)
                  (throw :--first-match rtn)
                (when rtn (push rtn acc))))
            (setq up (org-element-parent up)))
          (nreverse acc)))))
  (if (fboundp 'org-element-begin)
      (progn (declare-function org-element-begin "org-element")
             (declare-function org-element-end "org-element")
             (declare-function org-element-parent "org-element-ast")
             (defalias 'gptel-org--element-begin 'org-element-begin)
             (defalias 'gptel-org--element-end 'org-element-end)
             (defalias 'gptel-org--element-parent 'org-element-parent))
    (defsubst gptel-org--element-begin (node)
      "Get `:begin' property of NODE."
      (org-element-property :begin node))
    (defsubst gptel-org--element-end (node)
      "Get `:end' property of NODE."
      (org-element-property :end node))
    (defsubst gptel-org--element-parent (node)
      "Return `:parent' property of NODE."
      (org-element-property :parent node))))


;;; User options
(defcustom gptel-org-branching-context nil
  "Use the lineage of the current heading as the context for gptel in Org buffers.

This makes each same level heading a separate conversation
branch.

By default, gptel uses a linear context: all the text up to the
cursor is sent to the LLM.  Enabling this option makes the
context the hierarchical lineage of the current Org heading.  In
this example:

-----
Top level text

* Heading 1
heading 1 text

* Heading 2
heading 2 text

** Heading 2.1
heading 2.1 text
** Heading 2.2
heading 2.2 text
-----

With the cursor at the end of the buffer, the text sent to the
LLM will be limited to

-----
Top level text

* Heading 2
heading 2 text

** Heading 2.2
heading 2.2 text
-----

This makes it feasible to have multiple conversation branches."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-ignore-elements '(property-drawer)
  "Types of Org elements to be stripped from the prompt before sending.

By default gptel will remove Org property drawers from the
prompt.  For the full list of available elements, please see
`org-element-all-elements'.

Please note: Removing property-drawer elements is fast, but
adding elements to this list can significantly slow down
`gptel-send'."
  :group 'gptel
  :type '(repeat symbol))

(defcustom gptel-org-validate-link #'always
  "Validate links to be sent as context with gptel queries.

When `gptel-track-media' is enabled, this option determines if a
supported link will be followed and its source included with gptel
queries from Org buffers.  Currently only \"file\" and \"attachment\"
link types are supported (along with web URLs if the model supports
them).

It should be a function that accepts an Org link object and return
non-nil if the link should be followed.

By default, all links are considered valid.

Set this to `gptel-org--link-standalone-p' to only follow links placed
on a line by themselves, separated from surrounding text."
  :group 'gptel
  :type '(choice
          (const :tag "All links" always)
          (const :tag "Standalone links" gptel-org--link-standalone-p)
          (function :tag "Function")))

(defcustom gptel-org-convert-tables t
  "Whether to convert Markdown tables to Org tables in responses.

This only affects responses in Org buffers that are converted from
Markdown, see `gptel-org-convert-response'.

Markdown table delimiter rows are deleted and rows that are
missing their outer pipes get them, so that the table is valid Org
table syntax.  The columns are not aligned, see `org-table-align'."
  :group 'gptel
  :type 'boolean)

(defcustom gptel-org-response-headings 'outline
  "How to convert Markdown headings in LLM responses to Org.
`outline' (the default) converts them to Org headlines at the same
level, so the response becomes part of the buffer's outline.
`inline' converts them to inline bold text instead, so the model's
headings do not become outline structure (which would, for example,
restructure a gptel conversation saved in an Org buffer)."
  :group 'gptel
  :type '(choice (const :tag "Org headlines" outline)
                 (const :tag "Inline (bold) text" inline)))

(defcustom gptel-org-save-state-functions nil
  "Abnormal hook to save additional gptel state in Org buffers.

The functions in this hook are run at the end of
`gptel-org--save-state', after gptel has written its own properties.
They are called with no arguments, from the buffer being saved, with
the buffer widened and point at the beginning of the buffer, where
gptel's property drawer is available.  They are not run in Org
buffers that have no gptel state.

Use this to persist backend- or package-specific state (such as a
backend session ID) along with the rest of a gptel conversation.
Each function should write its own properties with `org-entry-put',
and should not signal: errors in hook functions are demoted, so they
cannot prevent the buffer from being saved.

For example:

  (defun my-backend-save-session-id ()
    \"Store the backend session ID in the gptel property drawer.\"
    (org-entry-put (point-min) \"GPTEL_MY_BACKEND_SESSION\"
                   my-backend-session-id))

  (add-hook \\='gptel-org-save-state-functions
            #\\='my-backend-save-session-id)

See `gptel-org-restore-state-functions' for the reverse."
  :group 'gptel
  :type 'hook)

(defcustom gptel-org-restore-state-functions nil
  "Abnormal hook to restore additional gptel state in Org buffers.

The functions in this hook are run at the end of
`gptel-org--restore-state', i.e. after gptel has restored its own
properties when `gptel-mode' is turned on.  They are called with no
arguments, from the buffer being restored, with the buffer widened and
point at the beginning of the buffer.  They are not run in Org buffers
that have no gptel state.

Use this to restore state saved by `gptel-org-save-state-functions',
typically by reading a property with `org-entry-get'.  Errors in hook
functions are demoted, so they cannot break the restoration of gptel's
own state."
  :group 'gptel
  :type 'hook)

(defcustom gptel-org-auto-enable nil
  "Whether to turn on `gptel-mode' in a gptel session file when it is visited.

gptel saves the state of a session in the Org file it was run in, but
it does not restore that state by itself when the file is visited
again: the state is restored by turning on `gptel-mode', as the
docstring of `gptel--save-state' says, so until now the only way to
resume a saved conversation was to put a file-local `eval' form in the
first line of the file:

  -*- eval: (gptel-mode 1) -*-

That is harmful for two reasons.  It changes the length of the first
line, and every buffer position stored in the file -- the response
bounds in the `GPTEL_BOUNDS' property -- is an absolute position, so
they all shift and end up pointing at the wrong text.  And a
file-local `eval' form runs arbitrary Elisp in any file that carries
it whenever `enable-local-variables' allows it, so an Org file from an
untrusted source can run code on your machine.

When this option is non-nil, `gptel-org--find-file-setup' turns on
`gptel-mode' in a visited file that already contains gptel session
state.  The file itself is never modified.  This happens after the
file has been re-read with the right coding system and after `so-long'
has been dealt with, see `gptel-org-restore-coding-system' and
`gptel-org-so-long-action'.

The default is nil because turning on a minor mode as a side effect of
visiting a file is a behavior change that should be opted into."
  :group 'gptel
  :type 'boolean)

(defcustom gptel-org-restore-coding-system 'utf-8
  "Coding system to re-read a gptel session file with, or nil to disable.

gptel writes session files as UTF-8, but Emacs does not always read
them back that way.  A single NUL byte anywhere in the file -- tool
output, a base64-encoded attachment, a model reply -- makes Emacs guess
`no-conversion', and then one byte of the file becomes one character of
the buffer.  The text is mangled, and worse, the response bounds gptel
stores in `GPTEL_BOUNDS' are character positions taken when the file
was written.  They no longer match the buffer, so restoring the session
either signals `args-out-of-range' or puts gptel's text properties on
the wrong text.

Re-reading the file with this coding system restores the
one-character-per-character mapping without editing any text, so no
stored position moves.  Only buffers that contain gptel session state
are re-read: a genuine binary file keeps whatever coding system Emacs
picked for it.

Set this to nil to leave the coding system of session files alone."
  :group 'gptel
  :type '(choice (coding-system :tag "Coding system" :value utf-8)
                 (const :tag "Do not re-read session files" nil)))

(defcustom gptel-org-so-long-action 'so-long-minor-mode
  "The `so-long' action for gptel session buffers, or nil to leave it alone.

`so-long' is part of Emacs since 27.1 and is meant to keep Emacs
responsive in buffers with very long lines.  Session files routinely
have them: tool output and base64-encoded attachments are single lines
of tens of thousands of characters.  The default `so-long' action,
`so-long-mode', replaces the major mode, and changing the major mode
kills all local variables, so `gptel-mode' is turned off and there is
no session left to restore.

With the default value, `so-long-minor-mode', a session buffer keeps
its major mode and its local variables, and `so-long' still applies its
performance measures: the variable overrides of
`so-long-variable-overrides' (line truncation, no bidirectional
reordering), the minor modes of `so-long-minor-modes' are turned off,
and fontification is disabled.  The value is a key of
`so-long-action-alist'; it is also assigned to `so-long-action'
locally in session buffers, so that a later automatic invocation of
`so-long' uses it as well.  (`so-long-action' is read when `so-long'
is called and is documented as suitable for a file- or dir-local
value, so setting it locally is supported.)

Note that `so-long' also makes the buffer read-only by default (see
`so-long-variable-overrides'), so a session buffer that `so-long' has
acted on is read-only until `so-long-revert' is called.

Set this to nil to leave `so-long' entirely alone, including not
undoing anything if it has already replaced the major mode of a
session buffer."
  :group 'gptel
  :type '(choice
          (const :tag "Keep the major mode (so-long-minor-mode)"
                 so-long-minor-mode)
          (const :tag "Leave so-long alone" nil)
          (symbol :tag "Another so-long action")))

(defconst gptel-org--link-regex
  (concat "\\(?:" org-link-bracket-re "\\|" org-link-angle-re "\\)")
  "Link regex for `gptel-mode' in Org mode.")

;; TODO(preset): This is a hack.  We need a better way to propagate the value of
;; the GPTEL_PRESET property all the way to the request augmentation block in
;; `gptel-request'.
(defvar gptel-org--presets nil
  "Presets specified in the Org property GPTEL_PRESET.
Meant for internal use only.")


;;; Setting context and creating queries
(defun gptel-org--get-topic-start ()
  "If a conversation topic is set, return it."
  (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
    (marker-position org-entry-property-inherited-from)))

(defun gptel-org-set-topic (topic)
  "Set a TOPIC and limit this conversation to the current heading.

This limits the context sent to the LLM to the text between the current
heading (i.e. the heading with the topic set) and the cursor position."
  (interactive
   (list
    (progn
      (or (derived-mode-p 'org-mode)
          (user-error "Support for multiple topics per buffer is only implemented for `org-mode'"))
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-entry-get nil "ITEM")))
                                 50))))))
  (when (stringp topic) (org-set-property "GPTEL_TOPIC" topic)))

;; NOTE: This can be converted to a cl-defmethod for
;; `gptel--create-prompt-buffer' (conceptually cleaner), but will cause
;; load-order issues in gptel.el and might be harder to debug.
(defun gptel-org--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided.  Its contents
depend on the value of `gptel-org-branching-context', which see."
  (when (use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (setq prompt-end (point-max)))
  (goto-char (or prompt-end (setq prompt-end (point))))
  (let ((topic-start (gptel-org--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if (and gptel-org-branching-context
             (or (fboundp 'org-element-lineage-map)
                 (prog1 nil
                   (display-warning
                    '(gptel org)
                    "Using `gptel-org-branching-context' requires Org version 9.7 or higher, it will be ignored."))))
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 ;; Collect all heading start positions in the lineage
                 (full-bounds (gptel-org--element-lineage-map
                                  (org-element-at-point) #'gptel-org--element-begin
                                '(headline) 'with-self) )
                 ;; lineage-map returns the full lineage in the unnarrowed
                 ;; buffer.  Remove heading start positions before (point-min)
                 ;; that are invalid due to narrowing, and add (point-min) if
                 ;; it's not already included in the lineage
                 (start-bounds
                  (nconc (cl-delete-if (lambda (p) (< p (point-min)))
                                       full-bounds)
                         (unless (save-excursion (goto-char (point-min))
                                                 (looking-at-p outline-regexp))
                           (list (point-min)))))
                 (end-bounds
                  (cl-loop
                   ;; (car start-bounds) is the begining of the current element,
                   ;; not relevant
                   for pos in (cdr start-bounds)
                   do (goto-char pos) (outline-next-heading)
                   collect (point) into ends
                   finally return (cons prompt-end ends))))
            (gptel--with-buffer-copy org-buf nil nil
              (cl-loop for start in start-bounds
                       for end in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              (goto-char (point-max))
              (gptel-org--unescape-tool-results)
              (gptel-org--strip-block-headers)
              (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                           (buffer-local-value 'gptel-org-ignore-elements
                                               org-buf)))
                (gptel-org--strip-elements))
              (when gptel-org--presets  ;let-bound to the Org prop GPTEL_PRESET
                (mapc (lambda (preset)       ;by `gptel-org--send-with-props'
                        (gptel--apply-preset
                         preset (lambda (sym val) (set (make-local-variable sym) val))))
                      gptel-org--presets))
              (setq org-complex-heading-regexp ;For org-element-context to run
                    (buffer-local-value 'org-complex-heading-regexp org-buf))
              (setq tab-width      ;Match source indentation for list parsing
                    (buffer-local-value 'tab-width org-buf))
              (current-buffer))))
      ;; Create prompt the usual way
      (let ((org-buf (current-buffer))
            (beg (point-min)))
        (gptel--with-buffer-copy org-buf beg prompt-end
          (gptel-org--unescape-tool-results)
          (gptel-org--strip-block-headers)
          (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                       (buffer-local-value 'gptel-org-ignore-elements
                                           org-buf)))
                (gptel-org--strip-elements))
          (when gptel-org--presets      ;let-bound to the Org prop GPTEL_PRESET
            (mapc (lambda (preset)           ;by `gptel-org--send-with-props'
                    (gptel--apply-preset
                     preset (lambda (sym val) (set (make-local-variable sym) val))))
                  gptel-org--presets))
          (setq org-complex-heading-regexp ;For org-element-context to run
                (buffer-local-value 'org-complex-heading-regexp org-buf))
          (setq tab-width      ;Match source indentation for list parsing
                (buffer-local-value 'tab-width org-buf))
          (current-buffer))))))

(defun gptel-org--strip-elements ()
  "Remove all elements in `gptel-org-ignore-elements' from the prompt."
  (let ((major-mode 'org-mode) element-markers)
    (if (equal '(property-drawer) gptel-org-ignore-elements)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            ;; ;; Slower but accurate
            ;; (let ((drawer (org-element-at-point)))
            ;;   (when (org-element-type-p drawer 'property-drawer)
            ;;     (delete-region (org-element-begin drawer) (org-element-end drawer))))

            ;; Fast but inexact, can have false positives
            (delete-region (match-beginning 0) (match-end 0))))
      ;; NOTE: Parsing the buffer is extremely slow.  Avoid this path unless
      ;; required.
      ;; NOTE: `org-element-map' takes a third KEEP-DEFERRED argument in newer
      ;; Org versions
      (org-element-map (org-element-parse-buffer 'element nil)
          gptel-org-ignore-elements
        (lambda (node)
          (push (list (gptel-org--element-begin node)
                      (gptel-org--element-end node))
                element-markers)))
      (dolist (bounds element-markers)
        (apply #'delete-region bounds)))))

(defun gptel-org--strip-block-headers ()
  "Remove all gptel-specific block headers and footers.
Every line that matches will be removed entirely.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start (literal "#+")
                (or (literal "begin") (literal "end"))
                (or (literal "_tool") (literal "_reasoning")))
            nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))))

(defun gptel-org--unescape-tool-results ()
  "Undo escapes done to keep results from escaping blocks.
Scans backward for gptel tool text property, then unescapes the block
contents."
  (save-excursion
    (goto-char (point-max))
    (let ((prev-pt (point)))
      (while (> prev-pt (point-min))
        (goto-char
         (previous-single-char-property-change (point) 'gptel))
        (let ((prop (get-text-property (point) 'gptel))
              (backward-progress (point)))
          (when (eq (car-safe prop) 'tool)
            ;; User edits to clean up can potentially insert a tool-call header
            ;; that is propertized.  Tool call headers should not be
            ;; propertized.
            (when (looking-at-p "[[:space:]]*#\\+begin_tool")
              (goto-char (match-end 0)))
            ;; TODO this code is able to put the point behind prev-pt, which
            ;; makes the region inverted.  The `max' catches this, but really
            ;; `read' and `looking-at' are the culprits.  Badly formed tool
            ;; blocks can lead to this being necessary.
            (org-unescape-code-in-region
             (min prev-pt (point)) prev-pt))
          (goto-char (setq prev-pt backward-progress)))))))

(defun gptel-org--link-standalone-p (object)
  "Check if link OBJECT is on a line by itself."
  (when-let* ((par (gptel-org--element-parent object))
              ((eq (org-element-type par) 'paragraph)))
    (and (= (gptel-org--element-begin object)
            (save-excursion
              (goto-char (org-element-property :contents-begin par))
              (skip-chars-forward "\t ")
              (point)))                 ;account for leading space before object
         (<= (- (org-element-property :contents-end par)
                (org-element-property :end object))
             1))))

(defsubst gptel-org--validate-link (link)
  "Validate an Org LINK as sendable under the current gptel settings.

Return a form (validp link-type path . REST), where REST is a list
explaining why sending the link is not supported by gptel.  Only the
first nil value in REST is guaranteed to be correct."
  (let ((mime))
    (if-let* ((link-type (org-element-property :type link))
              (resource-type
               (or (and (member link-type '("attachment" "file")) 'file)
                   (and (gptel--model-capable-p 'url)
                        (member link-type '("http" "https" "ftp")) 'url)))
              (path (org-element-property :path link))
              (user-check (funcall gptel-org-validate-link link))
              (readablep (or (eq resource-type 'url) ;Assume URLs are reachable
                             (file-remote-p default-directory)
                             (file-remote-p path)
                             (file-readable-p path)))
              (mime-valid
               (if (or (eq resource-type 'url)
                       (with-memoization
                           (alist-get (expand-file-name path)
                                      gptel--link-type-cache
                                      nil nil #'string=)
                         (gptel--file-binary-p path)))
                   (progn (setq mime (mailcap-file-name-to-mime-type path))
                          (gptel--model-mime-capable-p mime))
                 t)))
        (list t link-type path resource-type user-check readablep mime-valid mime)
      (list nil link-type path resource-type user-check readablep mime-valid mime))))

(cl-defmethod gptel--parse-media-links ((_mode (eql 'org-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward gptel-org--link-regex end t)
        (let* ((link (org-element-context))
               (link-status (gptel-org--validate-link link)))
          (cl-destructuring-bind
              (valid type path resource-type user-check readablep mime-valid mime)
              link-status
            (cond
             ((and valid (member type '("file" "attachment")))
              ;; Text file or supported binary file: collect text up to link
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (gptel-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              ;; collect link
              (push (if mime (list :media path :mime mime) (list :textfile path))
                    parts)
              (setq from-pt (point)))
             ((and valid (member type '("http" "https" "ftp")))
              ;; Collect text up to this image, and collect this image url
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (gptel-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              (push (list :url (org-element-property :raw-link link) :mime mime) parts)
              (setq from-pt (point)))
             ((not resource-type)
              (message "Link source not followed for unsupported link type \"%s\"." type))
             ((not user-check)
              (message (if (eq gptel-org-validate-link 'gptel--link-standalone-p)
                           "Ignoring non-standalone link \"%s\"."
                         "Link %s failed to validate, see `gptel-org-validate-link'.")
                       path))
             ((not readablep)
              (message "Ignoring inaccessible file \"%s\"." path))
             ((and (not mime-valid) (eq resource-type 'file))
              (message "Ignoring unsupported binary file \"%s\"." path))))))
      (unless (= from-pt end)
        (push (list :text (buffer-substring-no-properties from-pt end)) parts)))
    (nreverse parts)))

(defun gptel-org--annotate-links (beg end)
  "Annotate Org links whose sources will be sent with `gptel-send'.

Search between BEG and END."
  (when gptel-track-media
    (save-excursion
      (goto-char beg) (forward-line -1)
      (let ((link-ovs (cl-loop for o in (overlays-in (point) end)
                               if (overlay-get o 'gptel-track-media)
                               collect o into os finally return os)))
        (while (re-search-forward gptel-org--link-regex end t)
          (unless (gptel--in-response-p (1- (point)))
            (let* ((link (org-element-context))
                   (from (org-element-begin link))
                   (to (org-element-end link))
                   (link-status (gptel-org--validate-link link))
                   (ov (cl-loop for o in (overlays-in from to)
                                if (overlay-get o 'gptel-track-media)
                                return o)))
              (if ov                    ; Ensure overlay over each link
                  (progn (move-overlay ov from to)
                         (setq link-ovs (delq ov link-ovs)))
                (setq ov (make-overlay from to nil t))
                (overlay-put ov 'gptel-track-media t)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'priority -80))
              ;; Check if link will be sent, and annotate accordingly
              (gptel--annotate-link ov link-status))))
        (and link-ovs (mapc #'delete-overlay link-ovs))))
    `(jit-lock-bounds ,beg . ,end)))

(defun gptel-org--send-with-props (send-fun &rest args)
  "Conditionally modify SEND-FUN's calling environment.

If in an Org buffer under a heading containing a stored gptel
configuration, use that for requests instead.  This includes the
system message, model and provider (backend), among other
parameters.

ARGS are the original function call arguments."
  (if (derived-mode-p 'org-mode)
      (cl-destructuring-bind
          ( gptel-org--presets gptel-system-prompt gptel-backend
            gptel-model gptel-temperature gptel-max-tokens
            gptel--num-messages-to-send gptel-tools)
          (seq-mapn (lambda (a b) (or a b))
                    (gptel-org--entry-properties)
                    (list gptel-org--presets gptel-system-prompt gptel-backend
                          gptel-model gptel-temperature gptel-max-tokens
                          gptel--num-messages-to-send gptel-tools))
        (apply send-fun args))
    (apply send-fun args)))

(advice-add 'gptel-send :around #'gptel-org--send-with-props)
(advice-add 'gptel--suffix-send :around #'gptel-org--send-with-props)

;; ;; NOTE: Basic uses in org-mode are covered by advising gptel-send and
;; ;; gptel--suffix-send.  For custom commands it might be necessary to advise
;; ;; gptel-request instead.
;; (advice-add 'gptel-request :around #'gptel-org--send-with-props)


;;; Saving and restoring state
(defconst gptel-org--state-properties
  '("GPTEL_BOUNDS" "GPTEL_PRESET" "GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
    "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS" "GPTEL_NUM_MESSAGES_TO_SEND"
    "GPTEL_TOOLS")
  "Org properties that mark a buffer as holding gptel state.

These are the properties written by `gptel-org-set-properties' and
`gptel-org--save-state'.  The list is used both for looking properties
up with `org-entry-get' (see `gptel-org--properties-p') and for
searching the text of the buffer (see `gptel-org-session-p' and
`gptel-org--state-property-regexp'), so the two ways of asking whether
a buffer holds gptel state always agree.")

(defconst gptel-org--state-property-regexp
  (concat "^[ \t]*:" (regexp-opt gptel-org--state-properties) ":")
  "Regexp matching an Org property line that names gptel state.

See `gptel-org--state-properties' and `gptel-org-session-p'.")

(defun gptel-org-session-p (&optional buffer)
  "Return non-nil if BUFFER (default: current buffer) holds gptel session state.

A buffer holds gptel session state if it visits a file, and that file
contains the Org properties written by `gptel-org--save-state' (see
`gptel-org--state-properties'), which include the response bounds in
`GPTEL_BOUNDS'.

Unlike `gptel-org--properties-p', which asks Org for an entry property
and therefore needs `org-mode' and an actual property drawer, this
looks at the text of the buffer, so it works in any major mode.  That
is the point of it: it is called from `find-file-hook', where the major
mode may already have been replaced by something else -- typically
`so-long-mode', which `global-so-long-mode' can install before
`find-file-hook' runs at all.

It does not move point, does not depend on the major mode, and only
searches the first few kilobytes of the buffer, where the property
drawer is written."
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         (save-restriction
           (widen)
           (save-excursion
             (save-match-data
               (goto-char (point-min))
               ;; 4kB is plenty: gptel writes the drawer at `point-min'.
               (re-search-forward gptel-org--state-property-regexp
                                  (min (point-max) (+ (point-min) 4096))
                                  t)))))))

(defun gptel-org--properties-p (&optional pt)
  "Return non-nil if gptel state is stored as properties under PT.

PT defaults to the current position.  The properties checked are the
ones written by `gptel-org-set-properties' and
`gptel-org--save-state', see `gptel-org--state-properties'.

This needs `org-mode'; see `gptel-org-session-p' for a check that does
not."
  (let ((pt (or pt (point))))
    (cl-some (lambda (prop) (org-entry-get pt prop 'selective))
             gptel-org--state-properties)))

(defun gptel-org--entry-properties (&optional pt)
  "Find gptel configuration properties stored at PT."
  (pcase-let
      ((`(,presets ,system ,backend ,model ,temperature ,tokens ,num ,tools)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_PRESET" "GPTEL_SYSTEM" "GPTEL_BACKEND"
            "GPTEL_MODEL" "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
            "GPTEL_NUM_MESSAGES_TO_SEND" "GPTEL_TOOLS"))))
    (when presets
      (setq presets (mapcar #'intern (split-string presets))))
    (when system
      (setq system (string-replace "\\n" "\n" system)))
    (when backend
      (setq backend (alist-get backend gptel--known-backends
                               nil nil #'equal)))
    (when model (setq model (gptel--intern model)))
    (when temperature
      (setq temperature (gptel--to-number temperature)))
    (when tokens (setq tokens (gptel--to-number tokens)))
    (when num (setq num (gptel--to-number num)))
    (when tools
      (setq tools (cl-loop
                   for tname in (split-string tools)
                   for tool = (with-demoted-errors "gptel: %S"
                                (gptel-get-tool tname))
                   if tool collect tool else do
                   (display-warning
                    '(gptel org tools)
                    (format "Tool %s not found, ignoring" tname)))))
    (list presets system backend model temperature tokens num tools)))

(defun gptel-org--restore-state ()
  "Restore gptel state for Org buffers when turning on `gptel-mode'."
  (save-restriction
    (widen)
    (condition-case status
        (progn
          (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
            (gptel--restore-props (read bounds)))
          (pcase-let ((`(,presets ,system ,backend ,model ,temperature ,tokens ,num ,tools)
                       (gptel-org--entry-properties (point-min))))
            (dolist (preset presets)
              (if (gptel-get-preset preset)
                  (progn (gptel--apply-preset
                          preset (lambda (sym val) (set (make-local-variable sym) val)))
                         (setq gptel--preset preset))
                (display-warning
                 '(gptel presets)
                 (format "Could not activate gptel preset `%s' in buffer \"%s\""
                         preset (buffer-name)))))
            (when system (setq-local gptel-system-prompt system))
            (if backend (setq-local gptel-backend backend)
              (message
               (substitute-command-keys
                (concat
                 "Could not activate gptel backend \"%s\"!  "
                 "Switch backends with \\[universal-argument] \\[gptel-send]"
                 " before using gptel."))
               backend))
            (when model (setq-local gptel-model model))
            (when temperature (setq-local gptel-temperature temperature))
            (when tokens (setq-local gptel-max-tokens tokens))
            (when num (setq-local gptel--num-messages-to-send num))
            (when tools (setq-local gptel-tools tools))))
      (:success (message "gptel chat restored."))
      (error (message "Could not restore gptel state, sorry! Error: %s" status)))
    ;; Restore additional state written by extensions, see
    ;; `gptel-org-restore-state-functions'.
    (when (gptel-org--properties-p (point-min))
      (run-hook-wrapped
       'gptel-org-restore-state-functions
       (lambda (func)
         (with-demoted-errors "gptel-org-restore-state-functions: %S"
           (funcall func))
         nil)))))                       ;always return nil so the rest run

(defun gptel-org-set-properties (pt &optional msg)
  "Store the active gptel configuration under the current heading.

PT is the cursor position by default.  If MSG is non-nil (default),
display a message afterwards.

If a gptel preset has been applied in this buffer, a reference to it is
saved.

Additional metadata is stored only if no preset was applied or if it
differs from the preset specification.  This is limited to the active
gptel model and backend names, the system message, active tools, the
response temperature, max tokens and number of conversation turns to
send in queries.  (See `gptel--num-messages-to-send' for the last one.)

Additional properties can be stored by extensions, see
`gptel-org-save-state-functions' and
`gptel-org-restore-state-functions'."
  (interactive (list (point) t))
  (require 'gptel)
  (let ((preset-spec (and gptel--preset (gptel-get-preset gptel--preset))))
    (if preset-spec
        (org-entry-put pt "GPTEL_PRESET" (gptel--to-string gptel--preset))
      (org-entry-delete pt "GPTEL_PRESET"))

    ;; FIXME: nil can mean "no value was explicitly set by the user" as well as
    ;; "this setting has been set to nil".  We are not yet distinguishing
    ;; between the two when saving Org properties.  This is particularly
    ;; relevant for the system message, whose explicit nil value will not be
    ;; captured when saving Org buffers.

    ;; Model and backend
    (if (gptel--preset-mismatch-value preset-spec :model gptel-model)
        (org-entry-put pt "GPTEL_MODEL" (gptel--model-name gptel-model)))
    (if (gptel--preset-mismatch-value preset-spec :backend gptel-backend)
        (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend)))
    ;; System message
    (let ((parsed (car-safe (gptel--parse-directive gptel-system-prompt))))
      (if (gptel--preset-mismatch-value preset-spec :system parsed)
          (when parsed
            (org-entry-put pt "GPTEL_SYSTEM" (string-replace "\n" "\\n" parsed)))
        (org-entry-delete pt "GPTEL_SYSTEM")))
    ;; Tools
    (let ((tool-names (mapcar #'gptel-tool-name gptel-tools)))
      (if (gptel--preset-mismatch-value preset-spec :tools tool-names)
          (org-entry-put pt "GPTEL_TOOLS" (string-join tool-names " "))
        (org-entry-delete pt "GPTEL_TOOLS")))
    ;; Temperature, max tokens and cutoff
    (if (and (gptel--preset-mismatch-value preset-spec :temperature gptel-temperature)
             (not (equal (default-value 'gptel-temperature) gptel-temperature)))
        (org-entry-put pt "GPTEL_TEMPERATURE" (number-to-string gptel-temperature))
      (org-entry-delete pt "GPTEL_TEMPERATURE"))
    (if (and (gptel--preset-mismatch-value preset-spec :max-tokens gptel-max-tokens)
             gptel-max-tokens)
        (org-entry-put pt "GPTEL_MAX_TOKENS" (number-to-string gptel-max-tokens))
      (org-entry-delete pt "GPTEL_MAX_TOKENS"))
    (if (and (gptel--preset-mismatch-value
              preset-spec :num-messages-to-send gptel--num-messages-to-send)
             (natnump gptel--num-messages-to-send))
        (org-entry-put pt "GPTEL_NUM_MESSAGES_TO_SEND"
                       (number-to-string gptel--num-messages-to-send))
      (org-entry-delete pt "GPTEL_NUM_MESSAGES_TO_SEND")))
  (when msg
    (message "Added gptel configuration to current headline.")))

(defun gptel-org--save-state ()
  "Write the gptel state to the Org buffer as Org properties.

The response boundaries are buffer positions, so they are affected by
the length of the drawer they are stored in: writing the drawer moves
the text it points at.  They are therefore recomputed and rewritten
until the stored value is unchanged by its own write, which normally
takes two passes.  Saving an unmodified buffer again changes nothing,
so saving is idempotent."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (org-at-heading-p)
     (org-open-line 1))
   (gptel-org-set-properties (point-min))
   ;; Save response boundaries
   (let ((print-length))
     (cl-labels ((serialize ()
                   (prin1-to-string
                    (gptel--valid-bounds (gptel--get-buffer-bounds)))))
       (let ((serialized (serialize)))
         ;; With no responses to record, leave any existing drawer alone.
         (unless (equal serialized "nil")
           (let ((stored (org-entry-get (point-min) "GPTEL_BOUNDS" 'selective))
                 (passes 0))
             (while (and (< passes 4) (not (equal stored serialized)))
               (org-entry-put (point-min) "GPTEL_BOUNDS" serialized)
               (setq stored serialized
                     passes (1+ passes)
                     serialized (serialize)))
             ;; Give up instead of writing bounds that cannot be trusted.
             (unless (equal stored serialized)
               (display-warning
                '(gptel save-state)
                (format "gptel response bounds in buffer \"%s\" did not \
settle after %d passes, they may be off"
                        (buffer-name) passes))))))))
   ;; Save additional state written by extensions, see
   ;; `gptel-org-save-state-functions'.
   (when (gptel-org--properties-p (point-min))
     (run-hook-wrapped
      'gptel-org-save-state-functions
      (lambda (func)
        (with-demoted-errors "gptel-org-save-state-functions: %S"
          (funcall func))
        nil)))))                        ;always return nil so the rest run



;;; Visiting a saved session
;; The functions below each deal with one thing that can keep a visited session
;; file from being restored: the coding system Emacs guessed, `so-long'
;; replacing the major mode, and `gptel-mode' not being turned on.  They are
;; called, in that order, from `gptel-org--find-file-setup'.

(defun gptel-org--restore-coding ()
  "Re-read the current buffer if Emacs guessed a non-text coding system.

Session files are UTF-8, but a NUL byte in one makes Emacs read it with
a raw-text coding system (`no-conversion', `binary' or `raw-text'), one
byte per character, and then the response bounds stored in the file are
no longer character positions in the buffer.  Such a file is re-read
with `gptel-org-restore-coding-system'.  See that option for the
details.

Only buffers that hold gptel session state are touched, so a genuinely
binary file keeps its raw-text reading."
  (when (and gptel-org-restore-coding-system
             buffer-file-coding-system
             (memq (coding-system-type buffer-file-coding-system)
                   '(raw-text undecided))
             (gptel-org-session-p))
    ;; FORCE is non-nil so that the end-of-line convention comes from the file
    ;; as well, instead of from the (wrong) previous guess.
    ;;
    ;; Note that FORCE is *not* NOCONFIRM: `revert-buffer-with-coding-system'
    ;; calls `revert-buffer' with no arguments, which asks for confirmation
    ;; unless the file matches `revert-without-query'.  We are visiting a file
    ;; that is unmodified by definition, and a prompt in the middle of
    ;; `find-file-hook' would be both surprising and useless, so match
    ;; everything here.
    (let ((revert-without-query '("")))
      (revert-buffer-with-coding-system gptel-org-restore-coding-system t))))

(defun gptel-org--so-long-setup ()
  "Keep `so-long' from replacing the major mode of the current buffer.

Does nothing unless `gptel-org-so-long-action' is non-nil and `so-long'
is available.  See that option for what the problem is and for the
choice of action.

Two separate things can be needed, and both use only the public API
of `so-long':

- If `so-long' has already acted and replaced the major mode with
  `so-long-mode', the change is undone with `so-long-revert', which
  restores the original major mode, its minor modes and the overridden
  variables, and leaves `so-long-revert' usable afterwards.  The
  configured action is then applied with `so-long', which reverts the
  old action first if one is active and sets `so-long-function' and
  `so-long-revert-function' from the action it is given.

- Either way, the action is then recorded in the buffer-local value of
  `so-long-action', so a later automatic invocation of `so-long' in this
  buffer uses it too.  This has to happen last: reverting to the
  original major mode runs that mode, which kills all local variables,
  so a buffer-local `so-long-action' set earlier would not survive."
  (when (and gptel-org-so-long-action (require 'so-long nil t))
    (when (eq major-mode 'so-long-mode)
      (so-long-revert)
      (so-long gptel-org-so-long-action))
    (setq-local so-long-action gptel-org-so-long-action)))

(defvar gptel-org--in-find-file-setup nil
  "Non-nil while `gptel-org--find-file-setup' is running.

Re-reading a session file with the right coding system runs
`find-file-hook' again (the revert ends in `after-find-file'), so the
setup must not repeat itself for the same visit.")

(defun gptel-org--find-file-setup ()
  "Prepare the current buffer for holding a gptel session.

Runs from `find-file-hook' and does nothing in buffers that do not hold
gptel session state (see `gptel-org-session-p').  In a session buffer
it does three things, in this order:

1. Re-reads the file if Emacs guessed the wrong coding system, see
   `gptel-org-restore-coding-system'.  This comes first because
   re-reading a file runs `set-auto-mode', which can re-trigger
   `so-long' on the long lines that session files contain.
2. Keeps `so-long' from replacing the major mode, see
   `gptel-org-so-long-action'.
3. Turns on `gptel-mode', which restores the saved state, if
   `gptel-org-auto-enable' is non-nil.

Errors are demoted: a broken session file must not make visiting files
fail.

This function is added to `find-file-hook' when gptel-org is loaded, so
it applies from that point on.  gptel loads gptel-org lazily -- from
`gptel-mode' and `gptel--restore-state' -- so a session file visited
before gptel-org was first loaded is not set up; loading gptel-org
eagerly instead is not worth slowing startup down for.  Enable this
feature before visiting sessions by loading `gptel' and `gptel-org' at
startup."
  (unless gptel-org--in-find-file-setup
    (let ((gptel-org--in-find-file-setup t))
      (with-demoted-errors "gptel-org: Session setup failed: %S"
        (when (gptel-org-session-p)
          (gptel-org--restore-coding)
          (gptel-org--so-long-setup)
          (when (and gptel-org-auto-enable
                     (not (bound-and-true-p gptel-mode))
                     (derived-mode-p 'org-mode))
            (require 'gptel)
            (gptel-mode 1)))))))

(add-hook 'find-file-hook #'gptel-org--find-file-setup)


;;; Transforming responses
;; Markdown tables.  A Markdown pipe table is never valid Org table text: the
;; delimiter row (|---|:--:|) is a junk row in Org, and rows are only required
;; to have outer pipes in Org.  The converters below delete the delimiter row
;; and add the missing outer pipes.  They do not align columns -- `org-table-align'
;; is the user's business -- so the result is only *valid* Org table text.
;;
;; Both converters share one line-oriented pass,
;; `gptel-org--convert-table-lines', and the same deliberately textual
;; heuristics: a line that starts with a pipe is a table row, and a line that
;; merely contains a pipe is a row only when it adjoins the rest of a table.
;; Anything else is prose that happens to contain a pipe, and is left alone.

(defun gptel-org--table-delimiter-row-p (line)
  "Return non-nil if LINE is a Markdown table delimiter row.

A delimiter row contains nothing but pipes, hyphens, colons and
whitespace, with at least one pipe and one hyphen, e.g.
\"|---|---|\" or \"| :--- | ---: |\"."
  (and (string-match-p "\\`[ \t|:-]+\\'" line)
       (string-match-p "|" line)
       (string-match-p "-" line)))

(defun gptel-org--table-row-p (line)
  "Return non-nil if LINE starts like a table row.

That is, its first non-blank character is a pipe."
  (string-match-p "\\`[ \t]*|" line))

(defun gptel-org--table-bare-row-p (line)
  "Return non-nil if LINE could be a table row without outer pipes.

That is, it contains a pipe but does not start with one.  Prose
that mentions a pipe looks the same, so callers must check that
the line belongs to a table before converting it."
  (and (not (gptel-org--table-row-p line))
       (string-match-p "|" line)))

(defun gptel-org--src-block-line-p (line)
  "Return non-nil if LINE opens or closes a source block.

Markdown fences, and the Org block lines that
`gptel--replace-source-marker' turns them into, are both recognized."
  (string-match-p "\\`[ \t]*\\(?:```\\|#\\+\\(?:begin\\|end\\)_\\)" line))

(defun gptel-org--in-src-block-p (pos)
  "Return non-nil if POS is inside a source block.

The fences are counted from the beginning of the buffer, so the
answer does not depend on how much of the buffer the caller has
already seen.  This costs one regexp search per fence, not one per
line."
  (save-excursion
    (goto-char (point-min))
    (let ((fences 0))
      (while (re-search-forward "^[ \t]*\\(?:```\\|#\\+\\(?:begin\\|end\\)_\\)" pos t)
        (setq fences (1+ fences)))
      (cl-oddp fences))))

(defun gptel-org--table-ends-at-p (pos)
  "Return non-nil if the line before POS's line is part of a table.

A table row here means a line that starts with a pipe or a
Markdown delimiter row, so that a row without outer pipes is
recognized as such when it follows the rest of its table."
  (save-excursion
    (goto-char pos)
    (when (> (line-beginning-position) (point-min))
      (forward-line -1)
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (or (gptel-org--table-row-p line)
            (gptel-org--table-delimiter-row-p line))))))

(defun gptel-org--table-normalize-row (beg end)
  "Add the outer pipes missing from the table row between BEG and END.

BEG and END delimit the text of the row, without its newline."
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t" beg)
    (unless (eq (char-before) ?|) (insert " |"))
    (goto-char beg)
    (skip-chars-forward " \t" end)
    (unless (eq (char-after) ?|) (insert "| "))))

(defun gptel-org--convert-table-lines (start &optional final)
  "Convert the Markdown table lines between START and `point-max'.

Delimiter rows are deleted and rows that lack outer pipes get
them.  Source blocks are skipped, and a line that merely contains
a pipe is converted only when it adjoins a table, so prose
containing pipes is left alone.  The columns are not aligned, see
`org-table-align'.

Only complete lines are converted, since a streaming converter
cannot take back what it has already emitted: the last line is
converted only when FINAL is non-nil, and is otherwise returned as
the position from which output must be withheld until the rest of
it arrives.

The state this needs -- whether we are inside a source block, and
whether the preceding line is part of a table -- is read from the
buffer at START rather than carried between calls, so converting a
region twice is harmless."
  (let ((in-src (gptel-org--in-src-block-p start))
        (prev-table (gptel-org--table-ends-at-p start))
        (actions nil)
        hold)
    (save-excursion
      (goto-char start)
      (unless (bolp) (forward-line 1))  ;part of this line was already emitted
      (let ((end (point-max)))
        (while (if final (< (point) end) (< (line-end-position) end))
          (let* ((beg (line-beginning-position))
                 (eol (line-end-position))
                 (line (buffer-substring-no-properties beg eol))
                 (next (if (< eol end)
                           (save-excursion
                             (forward-line 1)
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                         "")))
            (cond
             ((gptel-org--src-block-line-p line)
              (setq in-src (not in-src) prev-table nil))
             (in-src (setq prev-table nil))
             ((gptel-org--table-delimiter-row-p line)
              (push (list 'delete beg eol) actions)
              (setq prev-table t))
             ((gptel-org--table-row-p line)
              (push (list 'row beg eol) actions)
              (setq prev-table t))
             ;; A row without outer pipes is only recognizable next to the rest
             ;; of a table: it continues one, or it heads one whose delimiter
             ;; row follows it.
             ((and (gptel-org--table-bare-row-p line)
                   (or prev-table (gptel-org--table-delimiter-row-p next)))
              (push (list 'row beg eol) actions)
              (setq prev-table t))
             (t (setq prev-table nil))))
          (forward-line 1))
        ;; A trailing partial line that already has a pipe in it can still turn
        ;; out to be a table row or a delimiter row: hold it back.
        (setq hold (and (not final)
                        (string-match-p "|" (buffer-substring-no-properties
                                             (point) end)))))
      ;; Apply the edits bottom-up so that the recorded positions stay valid.
      (dolist (action actions)
        (pcase-let ((`(,kind ,beg ,end) action))
          (if (eq kind 'delete)
              (delete-region beg (min (point-max) (1+ end)))
            (gptel-org--table-normalize-row beg end))))
      ;; The edits above moved the withheld line: it is now the last one.
      (when hold
        (setq hold (save-excursion (goto-char (point-max))
                                   (line-beginning-position)))))
    hold))

(defun gptel-org--inline-heading (heading)
  "Replace the Markdown heading at point with Org bold text.

HEADING is the string of hashes before point.  The text of the
heading is taken from the rest of the line, and an empty heading is
removed entirely."
  (let* ((beg (line-beginning-position))
         (text (string-trim (buffer-substring-no-properties
                             (+ beg (length heading)) (line-end-position)))))
    (delete-region beg (line-end-position))
    (insert (if (string-empty-p text) "" (concat "*" text "*")))))

;;;###autoload
(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
      (pcase (match-string 0)
        ;; Handle backticks
        ((and (guard (eq (char-before) ?`)) ticks)
         (gptel--replace-source-marker (length ticks))
         (save-match-data
           (catch 'block-end
             (while (search-forward ticks nil t)
               (unless (or (eq (char-before (match-beginning 0)) ?`)
                           (eq (char-after) ?`))
                 (gptel--replace-source-marker (length ticks) 'end)
                 (throw 'block-end nil))))))
        ;; Handle headings
        ((and (guard (eq (char-before) ?#)) heading)
         (cond
          ((looking-at "[[:space:]]")   ;Handle headings
           (if (eq gptel-org-response-headings 'inline)
               (gptel-org--inline-heading heading)
             (delete-region (line-beginning-position) (point))
             (insert (make-string (length heading) ?*))))
          ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
           (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
        ;; Handle emphasis
        ("**" (cond
               ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
               ;;  (delete-char 1))
               ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (or (= (point) 2)
                      (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                    (max (- (point) 2) (point-min))))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space:][:punct:]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (or (and (bobp) (looking-at "\\*[[:space:]]"))
                 (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
           ;; Bullet point, replace with hyphen
           (delete-char -1) (insert "-"))))))
    (when gptel-org-convert-tables
      (gptel-org--convert-table-lines (point-min) 'final))
    (buffer-string)))

(defun gptel--replace-source-marker (num-ticks &optional end)
  "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
  (let ((from (match-beginning 0)))
    (delete-region from (point))
    (if (and (= num-ticks 3)
             (save-excursion (beginning-of-line)
                             (skip-chars-forward " \t")
                             (eq (point) from)))
        (insert (if end "#+end_src" "#+begin_src "))
      (insert "="))))

;;;###autoload
(defun gptel--stream-convert-markdown->org (start-marker &optional info)
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream.

START-MARKER is used to identify the corresponding process when
cleaning up after.

INFO, if non-nil, is the info plist of the request being streamed.
The cleanup function for the converter's temporary buffer is added
to its `:post' list, so that the buffer and the hook entry are
freed when the request reaches a terminal state, even if no
response was ever inserted."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (in-org-src-block nil)
           (temp-buf ; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1
            (gptel--temp-buffer " *gptel-temp*"))
           (start-pt (make-marker))
           (ticks-total 0)      ;MAYBE should we let-bind case-fold-search here?
           (cleaned nil)
           (cleanup-fn
            (lambda (&rest args)
              ;; Called with (INFO) from the request's `:post' functions, and
              ;; with (BEG END) from `gptel-post-response-functions', where it
              ;; must not touch another request's state.
              (when (and (not cleaned)
                         (or (not (integerp (car args)))
                             (and (equal (car args) (marker-position start-marker))
                                  (eq (current-buffer) (marker-buffer start-marker)))))
                (setq cleaned t)
                (when (buffer-live-p (get-buffer temp-buf))
                  (kill-buffer temp-buf))
                (set-marker start-pt nil)
                (remove-hook 'gptel-post-response-functions cleanup-fn)))))
    (add-hook 'gptel-post-response-functions cleanup-fn)
    (when info
      (if (plist-member info :post)
          (plist-put info :post (cons cleanup-fn (plist-get info :post)))
        ;; Extend INFO in place: the caller keeps the head of the plist, so a
        ;; new one returned by `plist-put' would be thrown away.
        (let ((tail info))
          (while (cdr tail) (setq tail (cdr tail)))
          (setcdr tail (list :post (list cleanup-fn))))))
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer temp-buf)
          (save-excursion (goto-char (point-max)) (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (when in-src-block (setq ticks ticks-total))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
              (pcase (match-string 0)
                ("`"
                 ;; Count number of consecutive backticks
                 (backward-char)
                 (while (and (char-after) (eq (char-after) ?`))
                   (forward-char)
                   (if in-src-block (cl-decf ticks) (cl-incf ticks)))
                 ;; Set the verbatim state of the parser
                 (if (and (eobp)
                          ;; Special case heuristic: If the response ends with
                          ;; ^``` we don't wait for more input.
                          ;; FIXME: This can have false positives.
                          (not (save-excursion (beginning-of-line)
                                               (looking-at "^```$"))))
                     ;; End of input => there could be more backticks coming,
                     ;; so we wait for more input
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; We reached a character other than a backtick
                   (cond
                    ;; Ticks balanced, end src block
                    ((= ticks 0)
                     (progn (setq in-src-block nil)
                            (gptel--replace-source-marker ticks-total 'end)))
                    ;; Positive number of ticks, start an src block
                    ((and (> ticks 0) (not in-src-block))
                     (setq ticks-total ticks
                           in-src-block t)
                     (gptel--replace-source-marker ticks-total))
                    ;; Negative number of ticks or in a src block already,
                    ;; reset ticks
                    (t (setq ticks ticks-total)))))
                ;; Handle headings and misguided #+begin_src text
                ((and (guard (and (eq (char-before) ?#) (or (not in-src-block) in-org-src-block)))
                      heading)
                 (if in-org-src-block
                     ;; If we are inside an Org-style src block, look for #+end_src
                     (cond
                      ((< (- (point-max) (point)) 8) ;not enough information to close Org src block
                       (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                      ((looking-at "\\+end_src") ;Close Org src block
                       (setq in-src-block nil in-org-src-block nil)))
                   ;; Otherwise check for Markdown headings, or for #+begin_src
                   (cond
                    ((eobp)       ; Not enough information about the heading yet
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "[[:space:]]") ; Convert markdown heading to Org heading
                     (if (not (eq gptel-org-response-headings 'inline))
                         (progn (delete-region (line-beginning-position) (point))
                                (insert (make-string (length heading) ?*)))
                       ;; An inline heading is wrapped in bold text, which needs
                       ;; the whole line: wait for the rest of it.
                       (if (= (line-end-position) (point-max))
                           ;; NOTE: `match-beginning' is out of date here, see
                           ;; the `looking-at' above.
                           (progn (setq noop-p t)
                                  (set-marker start-pt (- (point) (length heading)))
                                  (goto-char (line-end-position)))
                         (gptel-org--inline-heading heading))))
                    ((< (- (point-max) (point)) 11) ;Not enough information to check if Org src block
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "\\+begin_src ") ;Overeager LLM switched to using Org src blocks
                     (setq in-src-block t in-org-src-block t)))))
                ;; Handle other chars: emphasis, bold and bullet items
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ;; TODO Not sure why this branch was needed
                  ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)") (delete-char 1))

                  ;; Looking back at "w**" or " **"
                  ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and "*" (guard (not in-src-block)))
                 (if (eobp)
                     ;; Not enough information about the "*" yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; "*" is either emphasis or a bullet point
                   (save-match-data
                     (save-excursion
                       (ignore-errors (backward-char 2))
                       (cond
                        ((and     ; At bob, underscore/asterisk followed by word
                          (or (and (bobp) (looking-at "\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                              (looking-at ; word followed by underscore/asterisk
                               "[^[:space:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                              (looking-at ; underscore/asterisk followed by word
                               "\\(?:[[:space:]]\\)\\(?:_\\|\\*\\)\\([^[:space:]]\\|$\\)"))
                          (not (looking-at "[[:punct:]]\\(?:_\\|\\*\\)[[:punct:]]")))
                         ;; Emphasis, replace with slashes
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "/"))
                        ((or (and (bobp) (looking-at "\\*[[:space:]]"))
                             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
                         ;; Bullet point, replace with hyphen
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "-"))))))))))
          ;; Convert the table lines that are complete, holding back an
          ;; incomplete one.  This must run after the scan above, which may
          ;; have set `noop-p' for an earlier construct.
          (when gptel-org-convert-tables
            (when-let* ((hold (gptel-org--convert-table-lines
                               (or (marker-position start-pt) (point-min)))))
              (set-marker start-pt
                          (if noop-p (min hold (marker-position start-pt)) hold))
              (setq noop-p t)))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'gptel-org)
;;; gptel-org.el ends here

;; Silence warnings about `org-element-type-p' and `org-element-parent', see #294.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
