android-with-emacs
==================

Notes and code for using Emacs for Android development.

Below I've added elisp code you can add to your <code>.emacs</code> file for <code>M-x compile</code>
support with emacs. The code provides the following features:

* Gradle build support
* Recognize compilation errors (jump to errors with <code>M-x `</code>

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x compile support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Traverses upwards from buffer-file-name and tries to locate the gradlew wrapper.
;; Return if found.
;;
;; Adapted from:
;; http://stackoverflow.com/questions/9037833/how-to-set-the-default-directory-of-compilation-in-emacs
(defun* get-closest-pathname (&optional (max-level 30) (file "gradlew"))
  (let ((root (expand-file-name "/"))
        (level 0))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       do (setq level (+ level 1))
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (> level max-level)
                       return nil
                       if (equal d root)
                       return nil))))

;; Tries to locate the gradlew wrapper, and if found create and return
;; a "make" string which changes into that directory and executes ./gradlew
;; with assembleDebug by default.
;;
;; How to recognize compilation errors.
;; For some reason, the errors returned when compilation is run from within emacs is:
;;   :TournmanApplication:compileRelease/home/marius/p/tournman/android/workspace/TournmanProject/TournmanApplication/src/main/java/net/kjeldahl/tournman/TournmanActivity.java:153: error: ';' expected
;;
;; This regexp captures the filename and line number by looking for ":compile.*?(filename):(lineno):
(require 'compile)
(add-hook 'java-mode-hook
          (lambda ()
            (unless (file-exists-p "gradlew")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name))
                         (mkfile (get-closest-pathname)))
                     (if mkfile
                         (progn (format "cd %s; ./gradlew assembleDebug"
                            (file-name-directory mkfile) mkfile))
                       ))))
            (add-to-list 'compilation-error-regexp-alist '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2))
            ))

```
