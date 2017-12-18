;; Before use make sure that you have node.js and npm, then run
;; $ sudo npm install -g tern
;; to install tern globally.

;; #############################################
;; ##### initializing package repositories #####
;; #############################################

(setq package-list '(js2-mode
		     js2-refactor
		     xref-js2
		     company-tern
		     avy
		     emmet-mode))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install-package)))

;; #############################################
;; ################ js2-mode ###################
;; #############################################

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; #############################################
;; ######### js2-refactor + xref-js2 ###########
;; #############################################

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with
;; xref, so unbind it

(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; #############################################
;; ################# tern  #####################
;; #############################################

(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))
;; Disable completeon keybindings, as wo use xref-js2 instead
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

;; #############################################
;; ################## avy  #####################
;; #############################################

(global-set-key (kbd "C-.") 'avy-goto-char)
(global-set-key (kbd "C-,") 'avy-goto-line)

;; #############################################
;; ################# emmet  ####################
;; #############################################

(add-hook 'html-mode-hook 'emmet-mode)

;; #############################################
;; ############## Ample themes  ################
;; #############################################

;; Comment/uncomment lines to toggle theme

;; (load-theme 'ample t t)
;; (enable-theme 'ample)

(load-theme 'ample-flat t t)
(enable-theme 'ample-flat)

;; (load-theme 'ample-light t t)
;; (enable-theme 'ample-light)

;; #############################################
;; ###### Disable menu/tool/scroll bars ########
;; #############################################

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; #############################################
;; ######### Disable blinking cursor ###########
;; #############################################

(blink-cursor-mode 0)

;; #############################################
;; ################# Custom ####################
;; #############################################
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (emmet-mode avy company-tern xref-js2 js2-refactor js2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
