;; -*- lexical-binding: t; -*-

(require 'dash)

(define-button-type 'help-find-keymap
  :supertype 'help-xref
  'help-function 'describe-keymap
  'help-echo (purecopy "mouse-2, RET: describe this keymap"))

;;;###autoload
(defun help-find-keybinding (keys)
  (interactive "MFind keybinding (kbd format): ")
  (let ((key-seq (kbd keys))
        (keymaps))
    (mapatoms (lambda (ob) (when (and (boundp ob)
                                      (keymapp (symbol-value ob))
                             (let ((lookup (lookup-key (symbol-value ob) key-seq)))
                               (when (and lookup
                                          (not (numberp lookup)))
                                 (push ob keymaps)))))))
    (if (< (length keymaps) 1)
        (message "%s not found in any keymap." keys)
      (help-setup-xref (list #'help-find-keybinding keys)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (propertize keys
                              'face 'help-key-binding
                              'font-lock-face 'help-key-binding))
          (princ " found in the following keymaps:\n\n")
          (princ "keymap")
          (indent-to 40)
          (princ "binding\n")
          (princ "------")
          (indent-to 40)
          (princ "-------\n")
          (--map
           (progn
             (let ((keymap-file-name (find-lisp-object-file-name it 'defvar)))
               (insert-text-button (symbol-name it)
                                   'type 'help-find-keymap
                                   'help-args (list it))
               (indent-to 40 1)
               (let ((lookup (lookup-key (symbol-value it) key-seq)))
                 (cond
                  ((symbolp lookup)
                   (cond
                    ((keymapp lookup)
                     (insert-text-button (symbol-name lookup)
                                         'type 'help-find-keymap
                                         'help-args (list lookup)))
                    ((functionp lookup)
                     (insert-text-button (symbol-name lookup)
                                         'type 'help-function
                                         'help-args (list lookup)))
                    (t
                     (insert-text-button (symbol-name lookup)
                                         'type 'help-symbol
                                         'help-args (list lookup)))))
                  (t
                   (cond
                    ((keymapp lookup)
                     (princ "Prefix Command"))
                    (t
                     (princ "??"))))))
               (princ "\n")))
           keymaps))))))

;;;###autoload
(defun help-find-function (fn)
  (interactive "aFind function: ")
  (message "help-find-function %s %s %s" fn (stringp fn) (symbolp fn))
  (let ((bindings (help-find--keymaps-lookup-function fn)))
    (if (< (length bindings) 1)
        (message "%s not found in any keymap." fn)
      (help-setup-xref (list #'help-find-function fn)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert-text-button (symbol-name fn)
                              'type 'help-function
                              'help-args (list fn))
          (princ " is found in the following keymaps:\n\n")
          (princ "keymap")
          (indent-to 40)
          (princ "binding\n")
          (princ "------")
          (indent-to 40)
          (princ "-------\n")
          (--map
           (-map
            (lambda (keys)
              (insert-text-button (symbol-name (car it))
                                  'type 'help-find-keymap
                                  'help-args (list (car it)))
              (indent-to 40 1)
              (insert (help--key-description-fontified keys))
              (princ "\n"))
            (cdr it))
           bindings))))))

(defun help-find--keymaps-lookup-function (fn)
  "Search for FN in all known keymaps."
  (let ((bindings))
    (mapatoms (lambda (ob) (when (and (boundp ob)
                                      (keymapp (symbol-value ob))
                                      (eq (intern (symbol-name ob)) ob)
                                      )
                             (let ((keymap-bindings (help-find--keymap-lookup-function
                                                     (symbol-value ob) fn)))
                               (when keymap-bindings
                                 (push (cons ob keymap-bindings)
                                       bindings)))))
              obarray)
    bindings))

(defun help-find--keymap-lookup-function (keymap fn)
  "Search KEYMAP for FN, recursively. FN should be a symbol."
  (let ((bindings))
    (map-keymap
     (lambda (ev binding)
       (cond
        ((symbolp binding)
         (cond
          ((eq binding fn)
           (push (list (list ev)) bindings))
          ((and (boundp binding)
            (keymapp (symbol-value binding)))
           (push (--map (cons ev it)
                        (help-find--keymap-lookup-function (symbol-value binding) fn))
                 bindings))))
        ((keymapp binding)
         (push (--map (cons ev it)
                      (help-find--keymap-lookup-function binding fn))
               bindings))))
     keymap)
    (apply #'-concat bindings)))

(provide 'help-find)
