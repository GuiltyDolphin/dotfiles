;;; Code:

(add-hook 'yesod-mode-hook (setq +YESOD-DIR+ (get-yesod-main-dir)))

(defvar yesod-mode-hook nil)

(defun hamlet-file-p ()
  (save-excursion
    (re-search-forward "widgetFile \"\\([^\"]+\\)\"" nil t)
    (match-string 1)))

(defun make-link-ham ()
  (let ((ham-name (hamlet-file-p)))
    (when ham-name
      (add-text-properties
       (match-beginning 1)
       (match-end 1)
       '(font-lock-face underline
              help-echo "yay?")))))

(if (hamlet-file-p)
          (add-text-properties
            (point)
            (save-excursion
              (dired-move-to-end-of-filename)
              (point))
            '(mouse-face highlight
              help-echo "mouse-2: visit this file in other window")))

 (defun dired-mouse-find-file-other-window (event)
       "In Dired, visit the file or directory name you click on."
       (interactive "e")
       (let ((window (posn-window (event-end event)))
             (pos (posn-point (event-end event)))
             file)
         (if (not (windowp window))
             (error "No file chosen"))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (setq file (dired-get-file-for-visit)))
         (if (file-directory-p file)
             (or (and (cdr dired-subdir-alist)
                      (dired-goto-subdir file))
                 (progn
                   (select-window window)
                   (dired-other-window file)))
           (select-window window)
           (find-file-other-window (file-name-sans-versions file t)))))

;(defun get-yesod-main-dir ()
;  (ido
  
(defun get-yesod-main-dir ()
  (let ((file (find-file-in-heirarchy (buffer-file-name) "Foundation.hs")))
    (if file
      (file-name-directory file)
      (message "Not in a Yesod directory!"))))
   
(defun yesod-hamlet-file ()
  (format "%s/templates/%s.hamlet" 
          +yesod-dir+
          (match-string-no-properties 1)))

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-file-in-heirarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR" 
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (find-file-in-heirarchy parent fname)))))


 (defun dired-mouse-find-file-other-window (event)
       "In Dired, visit the file or directory name you click on."
       (interactive "e")
       (let ((window (posn-window (event-end event)))
             (pos (posn-point (event-end event)))
             file)
         (if (not (windowp window))
             (error "No file chosen"))
         (with-current-buffer (window-buffer window)
           (goto-char pos)
           (setq file (dired-get-file-for-visit)))
         (if (file-directory-p file)
             (or (and (cdr dired-subdir-alist)
                      (dired-goto-subdir file))
                 (progn
                   (select-window window)
                   (dired-other-window file)))
           (select-window window)
           (find-file-other-window (file-name-sans-versions file t)))))
