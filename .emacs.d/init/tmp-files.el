;;--------------------------------;
;; Temp Files
;;--------------------------------;

;; stop emacs saving temp files in the directory hierarchy
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't bother with auto save and backups.
;; (setq auto-save-default nil)
(setq make-backup-files nil)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

