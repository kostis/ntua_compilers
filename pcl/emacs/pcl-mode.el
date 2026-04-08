;;; pcl-mode.el --- Major mode for the PCL programming language

(defconst pcl-keywords
  '("and" "array" "begin" "boolean" "char" "dispose" "div" "do"
    "else" "end" "false" "forward" "function" "goto" "if" "integer"
    "label" "mod" "new" "nil" "not" "of" "or" "procedure" "program"
    "real" "result" "return" "then" "true" "var" "while"))

(defconst pcl-builtin-functions
  '("writeInteger" "writeBoolean" "writeChar" "writeReal" "writeString"
    "readInteger" "readBoolean" "readChar" "readReal" "readString"
    "abs" "fabs" "sqrt" "sin" "cos" "tan" "arctan" "exp" "ln" "pi"
    "trunc" "round" "ord" "chr"))

(defconst pcl-font-lock-keywords
  (list
   `(,(regexp-opt pcl-keywords 'words) . font-lock-keyword-face)
   `(,(regexp-opt pcl-builtin-functions 'words) . font-lock-builtin-face)
   '("\\(program\\|procedure\\|function\\)\\s-+\\([a-zA-Z][a-zA-Z0-9_]*\\)"
     (2 font-lock-function-name-face))
   '(":\\s-*\\(\\^*\\s-*\\(integer\\|real\\|boolean\\|char\\|array\\)\\)"
     (1 font-lock-type-face))
   '("'\\([^'\\\\]\\|\\\\.\\)'" . font-lock-string-face)
   '("\\b[0-9]+\\(\\.[0-9]+\\([eE][+-]?[0-9]+\\)?\\)?\\b"
     . font-lock-constant-face)))

(defvar pcl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `pcl-mode'.")

(defun pcl-comment-syntax ()
  "Set up (* *) comment syntax for PCL."
  (setq-local comment-start "(*")
  (setq-local comment-end "*)"))

(defun pcl-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("(\\(\\*\\)" (1 "< b"))   ; (* opens comment
    ("\\(\\*\\))" (1 "> b")))  ; *) closes comment
   start end))

(define-derived-mode pcl-mode prog-mode "PCL"
  "Major mode for editing PCL programs."
  :syntax-table pcl-mode-syntax-table
  (setq-local font-lock-defaults '(pcl-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (pcl-comment-syntax)
  (setq-local syntax-propertize-function #'pcl-syntax-propertize))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pcl\\'" . pcl-mode))

(provide 'pcl-mode)
;;; pcl-mode.el ends here
