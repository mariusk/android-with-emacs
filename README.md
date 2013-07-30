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

Auto-complete and searching imports
-----------------------------------

I haven't find a really simple setup yet; there are many ways. Unfortunately the documentation for how do to this
with Emacs is almost non-existent. But I have something that should be fairly simple to get started with, at least
if you are using emacs24 (which I am). First you need to install the `auto-complete` and `ggtags` packages.
The auto-complete package seem to offer auto complete for both local symbols (defined in your source code) and
some import classes, but I am not sure exactly how it figures out that I'm using Android, and where my Android sources/jars are
located.

The `ggtags` package is an interface to the GNU Global package for navigating source code, which you also need
to install. On Ubuntu, you can do this using `sudo apt-get install global`. After installing, navigate to your project
root and create links to any other source directories you would like indexed. You can put these links in a separate
subdirectory; the `gtags` program (which is installed when you installed `global`), searches for sources in all
subdirs. For android development I've created a "libsrc" directory, where I have a symlink to
`/opt/android-studio/sdk/sources/android-18/`. With this in place, just execute `gtags` in your project root and
the program will create the files `GTAGS`, `GSYMS` and `GRTAGS` in your project root. These are index files
used for looking up source code references. The `global` system can be used from the command line, for instance
execute `global Activity` in your project and it will show you references to the Android source where the class
gets defined and used.

More interestingly, with the `ggtags` package installed you can do this from within emacs. If you load up a java
source file from your project, move the cursor or write `Activity` somewhere, and then press M-., it will open
up two buffers; one which shows the same references that it showed when you executed `global` from the command
line, and in the other it will open up the actual first file. In this case this is the Android source code for
the implementation of Activity. M-* will close these windows again, while M-{ and M-} will navigate to the
next/previous mention of `Activity` (for this search). From this, figuring out the correct `import` to put
in place is easy (move to the top of the file or look at the buffer file name). While not as easy as pressing
a magic button to get an import statement generated and put into your source code file, it is possibly more
useful as it allows you to easily get to both the docs and source code easily.

Ah, I almost forgot, as with most things emacs, you may have to add some stuff to your .emacs file. Here's what I
have in mine which seems to work fine:

    (require 'ggtags)
    (autoload 'ggtags-mode "ggtags" "" t)
    (setq gtags-suggested-key-mapping t)
    (global-set-key (kbd "C-c C-f") 'gtags-find-file)
    
    (require 'auto-complete-config)
    (ac-config-default)
    (setq ac-ignore-case 'smart)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)
