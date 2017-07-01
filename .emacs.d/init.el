;;; package --- Summary
;;; Commentary:
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; smart-line
;; https://github.com/Malabarba/smart-mode-line
(defvar sml/no-confirm-load-theme)
(defvar sml/theme)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)

;; auto-complete
;; ref http://keisanbutsuriya.hateblo.jp/entry/2015/02/08/175005
;; ref http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part9

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; display time
;; ref https://gist.github.com/kmakita13714/5186913

;; 以下の書式に従ってモードラインに日付・時刻を表示する
(defvar display-time-string-forms)
(setq display-time-string-forms
      '((format "%s/%s/%s(%s) %s:%s" year month day dayname 24-hours minutes)
	load
	(if mail " Mail" "")))
;; 時刻表示の左隣に日付を追加。
(defvar display-time-kawakami-forms)
(setq display-time-kawakami-forms t)
;; 24時間制
(defvar display-time-24hr-format)
(setq display-time-24hr-format t)
;; 時間を表示
(display-time)

;; bind backspace to C-h
(keyboard-translate ?\C-h ?\C-?)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; automatically fix buffer.
(global-auto-revert-mode 1)

;; http://konbu13.hatenablog.com/entry/2014/01/15/223014
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688

(helm-mode 1)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)


;; (define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)


(set-face-attribute 'helm-selection nil
		    :background "purple"
		    :foreground "black")



;; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b#1-2
(add-hook 'after-init-hook #'global-flycheck-mode)

;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(global-anzu-mode +1)

;; ref http://qiita.com/biwakonbu/items/15a9a58cbb2b618a0069
;;; メニューバーを非表示
;; M-x menu-bar-mode で表示非表示を切り替えられる
(menu-bar-mode -1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

;; http://www.helptouser.com/computer/576953-how-do-i-show-the-git-status-in-the-emacs-bottom-bar.html

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
	(concat ad-return-value
		(let ((plus-minus (vc-git--run-command-string
				   file "diff" "--numstat" "--")))
		  (and plus-minus
		       (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
		       (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))


;; go-mode

(add-hook 'go-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    (setq tab-width 4)
	    (setq indent-tabs-mode 1)))

;; irony

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's asynchronous function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (php-mode yasnippet yaml-mode web-mode volatile-highlights use-package smex smartparens smart-mode-line-powerline-theme rinari puppet-mode projectile prodigy popwin pallet nyan-mode multiple-cursors markdown-mode magit irony idle-highlight-mode htmlize helm go-mode gitignore-mode gitconfig-mode flymake-yaml flymake-cppcheck flycheck-cask expand-region exec-path-from-shell drag-stuff csv-mode cmake-mode auto-complete anzu))))
