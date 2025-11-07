;;; base16-wal.el ---


;;; Commentary:
;;

(require 'base16-theme)
(require 'map)

;;; Code:
(deftheme base16-custom)

(defvar color-filepath "~/.cache/flavours/colors.json")
(setq base16-colors
      (with-temp-buffer
	(insert-file-contents color-filepath)
	(goto-char (point-min))
	(json-parse-buffer :object-type 'plist)))

(base16-theme-define 'base16-custom base16-colors)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'base16-custom)
;;; base16-wal.el ends here
