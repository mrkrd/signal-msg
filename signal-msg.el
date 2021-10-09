;;; signal-msg.el --- Send Signal messages from GNU Emacs using signal-cli


;;; Commentary:

;; This pakcages sends Signal messages using signal-cli command line
;; client.

;; https://github.com/mrkrd/signal-msg

;;; Code:


(require 'json)



(defcustom signal-msg-username nil "Signal phone number with plus sign and the country code.")



(defun signal-msg--get-data-json ()
  (let ((data-file (expand-file-name
                    (concat "~/.local/share/signal-cli/data/"
                            signal-msg-username ".d/recipients-store"))))
    (with-temp-buffer
      (insert-file-contents data-file)
      (json-read)
      )
    ))

;; (signal-msg--get-data-json)


(defun signal-msg--recipients-to-label-and-number (recipient)
  (let* ((number (alist-get 'number recipient))
         (contact (alist-get 'contact recipient))
         (name (alist-get 'name contact)))
    (cons (concat name " " number) number)
  ))


(defun signal-msg--labels-and-numbers ()
  "Return an alist of labels (concat of name and number) → phone numbers."
  (let* ((data (signal-msg--get-data-json))
         (recipients (alist-get 'recipients data))
         )
    (mapcar 'signal-msg--recipients-to-label-and-number recipients)
    ))

;; (signal-msg--labels-and-numbers)


;; Below (commented out) is the implementation that gets a list of
;; contacts from the CLI tool.  However, it's too slow, so the
;; implementation that parses internal JSON store of signal-cli is
;; enabled instead.
;;
;; (defun signal-msg--parse-contact-list ()
;;   (let (name-tel)
;;     (goto-char (point-min))
;;     (while (re-search-forward
;;             (rx line-start
;;                 "Number: "
;;                 (group "+" (one-or-more digit)) " Name: "
;;                 (group (zero-or-more nonl))
;;                 " Blocked: " (or "false" "true")
;;                 )
;;             nil t)
;;       (setq name-tel
;;             (append name-tel (list (cons (match-string 2) (match-string 1)))))
;;       )
;;     (mapcar (lambda (c) (cons (concat (car c) " " (cdr c)) (cdr c))) name-tel)
;;     )
;;   )
;;
;; (defun signal-msg--labels-and-numbers ()
;;   (with-temp-buffer
;;     (progn
;;       (call-process
;;        "signal-cli"
;;        nil                                    ; infile
;;        (current-buffer)                       ; destination
;;        nil                                    ; display
;;        "-u" signal-msg-username "listContacts"
;;        )
;;       (signal-msg--parse-contact-list)
;;       )))


;; (signal-msg--labels-and-numbers)



(defvar signal-msg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'signal-msg-send)
    (define-key map (kbd "C-c C-k") 'signal-msg-cancel)
    map)
  "Keymap for `signal-msg-mode'.")


(define-derived-mode signal-msg-mode text-mode "Signal"
  "Signal Message Mode"
  )


(defun signal-msg-send ()
  (interactive)
  (let ((exit-code (call-process-region
                    (point-min)
                    (point-max)
                    "signal-cli"
                    nil                                  ; delete
                    nil                                  ; buffer
                    nil                                  ; display
                    "-u" signal-msg-username "send" signal-msg-dest-number
                    )))
    (if (= exit-code 0)
        (kill-buffer)
      (warn (format "Something went wrong. signal-cli returned %d" exit-code)))
    ))


(defun signal-msg-cancel ()
  (interactive)
  (when (y-or-n-p "Cancel? ")
    (kill-buffer))
  )


(defun signal-msg-new-message ()
  (interactive)
  (let* ((labels-and-numbers (signal-msg--labels-and-numbers))
         (label (completing-read "Signal Contact: " labels-and-numbers nil t))
         (number (cdr (assoc-string label labels-and-numbers)))
         (buf-name (format "*new signal msg to %s*" label))
         (buffer (generate-new-buffer buf-name))
         )
    (switch-to-buffer buffer)
    (signal-msg-mode)
    (turn-off-auto-fill)
    (visual-line-mode 1)
    (setq-local signal-msg-dest-number number)
    ))

;; (signal-msg-new-message)


(provide 'signal-msg)

;;; signal-msg.el ends here
