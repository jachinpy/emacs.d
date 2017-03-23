;;; package --- tramp;
;;; Commentary:
;;; Code:

(setq tramp-default-method "ssh")

;; use '/ssh::/' goto ubuntu host home dir(virtual env or openstack).
;; edit /etc/hosts  {IP  openstack.dev.com}
(setq tramp-default-user "ubuntu"
      tramp-default-host "openstack.dev.com")

(defun remote_shell ()
  "M-x remote_shell, goto ssh:ubuntu/openstack.dev.com default."
  (interactive)
  (let ((default-directory "/ssh::~/"))
    (shell)))

(provide 'init-tramp)
;;; init-tramp ends here
