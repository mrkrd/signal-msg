;;; signal-msg.el --- Send Signal messages with signal-cli


;;; Commentary:

;; This pakcages sends Signal messages using signal-cli command line
;; client.

;; https://github.com/mrkrd/signal-msg

;;; Code:


(require 'ivy)
(require 'json)



(defcustom signal-msg-username nil "Signal phone number with plus sign and the country code.")


(defun signal-msg--get-data-json ()
  (let ((data-file (expand-file-name (concat "~/.config/signal/data/"
                                             signal-msg-username))))
    (with-temp-buffer
      (insert-file-contents data-file)
      (json-read)
      )
    ))

;; (signal-msg--get-data-json)


(defun signal-msg--contact-to-line (contact)
  "Convert a contact alist to a cons cell with (name-number . number)."
  (let* ((name (alist-get 'name contact))
         (number (alist-get 'number contact))
         )
    (cons (concat name ": " number)  number)
    ))

;; (signal-msg--contact-to-line '((name . "Some Name") (number . "+123456")))


(defun signal-msg--lines-and-numbers ()
  (let* ((data (signal-msg--get-data-json))
         (contact-store (alist-get 'contactStore data))
         (contacts (alist-get 'contacts contact-store))
         )
    (mapcar 'signal-msg--contact-to-line contacts)
    ))


;; (signal-msg--lines-and-numbers)


(defun signal-msg--select-number ()
  (let* ((lines-and-numbers (signal-msg--lines-and-numbers))
         (lines (mapcar 'car lines-and-numbers))
         (line (ivy-read "Contact: " lines))
         (number (cdr (assoc line lines-and-numbers)))
         )
    number
    ))

;; (signal-msg--select-number)


(defun signal-msg-set-number ()
  (let ((number (signal-msg--select-number)))
    (setq-local signal-msg-dest-number number)
    ))

;; (signal-msg-set-number)


(defvar signal-msg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'signal-msg-send)
    (define-key map (kbd "C-c C-k") 'kill-buffer)
    map)
  "Keymap for `signal-msg-mode'.")


(define-derived-mode signal-msg-mode text-mode "Signal"
  "Signal Message Mode"
  )


(defun signal-msg-send ()
  (interactive)
  (call-process-region
   (point-min)
   (point-max)
   "signal-cli"
   nil                                  ;delete
   nil                                  ;destination
   nil                                  ;display
   "-u" signal-msg-username "send" signal-msg-dest-number
   )
  (kill-buffer)
  )


(defun signal-msg-new-message ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new singal message*"))
        )
    (switch-to-buffer buffer)
    (signal-msg-mode)
    (signal-msg-set-number)
    ))

;; (signal-msg-new-message)


(provide 'signal-msg)

(provide 'signal-msg)

;;; signal-msg.el ends here
