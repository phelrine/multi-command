;; (require 'multi-command)
;; (global-set-key (kbd "C-q") 'mcmd-set-next-mode)
;; (define-multi-command global-map "C-f" '(forward-char forward-word) :reset t)
;; (define-multi-command global-map "C-b" '(backward-char backward-word) :reset t)

(eval-when-compile (require 'cl))

(defstruct mcmd-command mode default selectables reset)
(defvar mcmd-hash-table (make-hash-table :test 'equal))

(defun mcmd-get-hash (name)
  (gethash (if (symbolp name) (symbol-name name) name) mcmd-hash-table))

(defun mcmd-get-mode (name)
  (let ((cmd (mcmd-get-hash name)))
    (if cmd (mcmd-command-mode cmd) 0)))

(defun mcmd-set-mode (name mode)
  (let ((cmd (mcmd-get-hash name)))
    (if cmd (setf (mcmd-command-mode cmd)
                  (mod mode (length (mcmd-command-selectables cmd)))))))

(defun mcmd-set-default (name mode)
  (let ((cmd (mcmd-get-hash name)))
    (if cmd (setf (mcmd-command-default cmd)
                  (mod mode (length (mcmd-command-selectables cmd)))))))

(defun mcmd-set-mode-interactively (mode)
  (interactive "p")
  (mcmd-set-mode last-command mode))

(defun mcmd-set-next-mode ()
  (interactive)
  (let ((cmd (mcmd-get-hash last-command)))
    (if cmd (mcmd-set-mode last-command (+ (mcmd-command-mode cmd) 1)))))

(defmacro* define-multi-command (map key selectables &key reset)
  (let* ((cmd (gensym))
         (funcname (concat "mcmd-" key))
         (funcsym (intern funcname)))
    `(progn
       (puthash ,funcname
                (make-mcmd-command :mode 0 :default 0 :selectables ,selectables :reset ,reset)
                mcmd-hash-table)
       (defun ,funcsym ()
         (interactive)
         (let ((,cmd (mcmd-get-hash ,funcname)))
           (and ,cmd
                (mcmd-command-reset ,cmd)
                (not (eq last-command this-command))
                (not (eq last-command #'mcmd-set-next-mode))
                (not (eq last-command #'mcmd-set-mode-interactively))
                (setf (mcmd-command-mode ,cmd) (mcmd-command-default ,cmd))))
         (call-interactively
          (nth (mod (mcmd-get-mode ,funcname) ,(length selectables)) ,selectables)))
       (define-key ,map ,(read-kbd-macro key) ',funcsym))))
;; (prin1 (macroexpand '(define-multi-command global-map "C-f" '(forward-char forward-word))))

(defvar anything-c-source-multi-command
  '((name . "multi-command")
    (candidates . (lambda () (loop for k being the hash-keys in mcmd-hash-table collect k)))
    (real-to-display . anything-c-mcmd-real-to-display)
    (action . anything-mcmd-change-default)))

(defun anything-c-mcmd-real-to-display (name)
  (let ((cmd (mcmd-get-hash name)))
    (concat (replace-regexp-in-string "^mcmd-" "" name) "\t\t"
            (symbol-name (nth (mcmd-command-mode cmd)
                              (mcmd-command-selectables cmd))))))

(defun anything-mcmd-change-default (name)
  (if (mcmd-get-hash name)
      (let* ((cmd (mcmd-get-hash name))
             (selectables (mcmd-command-selectables cmd)))
        (anything-other-buffer
         `((name . "change default mode")
           (candidates . selectables)
           (action . (lambda (func)
                       (let ((index (position (intern func) selectables)))
                         (mcmd-set-default ,name index)
                         (mcmd-set-mode ,name index)))))
         "*anything multi-command*"))))

(defun anything-multi-command ()
  (interactive)
  (if (fboundp 'anything-other-buffer)
      (anything-other-buffer '(anything-c-source-multi-command) "*anything multi-command*")))

(provide 'multi-command)
