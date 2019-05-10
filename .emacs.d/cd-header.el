;;; cd-header --- Automatic file headers

;;; Commentary:

;;; Code:
(setq auto-insert-alist
      '(((emacs-lisp-mode . "Emacs lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         "(provide '" (substring (file-name-nondirectory buffer-file-name) 0 -3) ")\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Description: " _ "\n"
         " */\n\n")
        ((shell-mode . "Shell script") nil
         "#!/bin/bash\n\n"
         " # File: " (file-name-nondirectory buffer-file-name) "\n"
         " # Description: " _ "\n\n")))

(provide 'cd-header)
;;; cd-header ends here
