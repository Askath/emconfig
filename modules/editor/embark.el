;; Context menu for consult?
;; act on findings similar to a right click


;; (use-package embark
;;   :ensure t
;;   :demand t
;;   :after avy
;;   :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
;;   :init
;;   ;; Add the option to run embark when using avy
;;   (defun bedrock/avy-action-embark (pt)
;;     (unwind-protect
;;         (save-excursion
;;           (goto-char pt)
;;           (embark-act))
;;       (select-window
;;        (cdr (ring-ref avy-ring 0))))
;;     t)

;;   ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
;;   ;; candidate you select
;;   (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

;; (use-package embark-consult
;;   :ensure t)
