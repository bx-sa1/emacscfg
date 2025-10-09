;;; base16-wal.el ---


;;; Commentary:
;;

(require 'base16-theme)
(require 'map)

;;; Code:
(deftheme base16-wal)

(defvar wal-color-filepath "~/.cache/wal/colors.json")
(setq base16-wal-colors
      (let* ((json (with-temp-buffer
		     (insert-file-contents wal-color-filepath)
		     (goto-char (point-min))
		     (json-parse-buffer :object-type 'plist)))
	     (colors (plist-get json :colors)))
	(flatten-tree
	 (map-apply
	  (lambda (k v)
	    (list
	     (intern (concat ":base" (format "%02X" (string-to-number (substring (symbol-name k) 6)))))
	     v))
	  colors))))

(base16-theme-define 'base16-wal base16-wal-colors)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'base16-wal)
;;; base16-wal.el ends here
