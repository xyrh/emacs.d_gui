(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(setq-default cursor-type 'bar)
(setq inhibit-splash-screen t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(electric-pair-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-diff-hl-mode)

(setq c-default-style "linux")
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(setq mouse-yank-at-point t)
(setq ring-bell-function 'ignore)

(defun show-buffer-file-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name)))))
(global-set-key (kbd "C-c C-f") 'show-buffer-file-name)

(setq url-automatic-caching t)
(global-set-key (kbd "C-c i") 'youdao-dictionary-search-from-input)
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(global-set-key (kbd "C-c p") 'youdao-dictionary-play-voice-at-point)

(global-set-key (kbd "M-o") 'ace-window)

(global-set-key (kbd "C-\\") #'helm-imenu-anywhere)

(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)

(avy-setup-default)
(global-set-key (kbd "M-s a") 'avy-goto-char)
(global-set-key (kbd "M-s l") 'avy-goto-line)

(avy-setup-default)
(global-set-key (kbd "M-g a") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)

(require 'popwin)
(popwin-mode t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c k") 'counsel-ag)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(global-company-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(load-theme 'molokai t)

(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-select-mode-hook
	  '(lambda ()
	     (setq hl-line-face 'underline)
	     (hl-line-mode 1)))
(add-hook 'c-mode-hook
	  '(lambda ()
	     (gtags-mode 1)))
(setq gtags-suggested-key-mapping t)
(setq gtags-auto-update t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default)))
 '(package-selected-packages
   (quote
    (popwin fiplr imenu-anywhere helm ace-window youdao-dictionary counsel-gtags avy counsel ivy swiper diff-hl company molokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
