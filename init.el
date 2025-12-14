
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Указываем используемые пакеты.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Кодировка языковая.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-language-info-alist
 "Windows-1251" `((charset cp1251)
		  (coding-system cp1251)
		  (coding-priority cp1251)
		  (nonascii-translation . cp1251)
		  (input-method . "russian-typewriter")
		  (features cyril-util)
		  (unibyte-display . cp1251)
		  (sample-text . "Russian (Русский)	Здравствуйте!")
		  (documentation . "Support for Windows cp1251."))
 '("Cyrillic"))

(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Путь к лисповым исходникам.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Подсветка символов выходящих за допустимую ширину строки.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
;(autoload 'flex-mode "flex-mode" "Flex Mode" t)
(autoload 'column-enforce-mode
  "column-enforce-mode"
  "Highlight text that extends beyong a column"
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройки под разные языки.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append (list (cons "\\.sp$"   'spice-mode)
                    (cons "\\.cir$"  'spice-mode)
                    (cons "\\.ckt$"  'spice-mode)
                    (cons "\\.mod$"  'spice-mode)
                    (cons "\\.cdl$"  'spice-mode)
                    (cons "\\.chi$"  'spice-mode)
                    (cons "\\.inp$"  'spice-mode)
                    (cons "\\.scs$"  'spice-mode)
                    (cons "\\.a51$"  'asm-mode)
                    (cons "\\.cc$"   'c++-mode)
                    (cons "\\.c$"   'c-mode)
                    (cons "\\.cpp$"  'c++-mode)
                    (cons "\\.cxx$"  'c++-mode)
                    (cons "\\.h$"    'c++-mode)
                    (cons "\\.tcc$"  'c++-mode)
                    (cons "\\.icc$"  'c++-mode)
                    (cons "\\.il$"   'lisp-mode)
                    (cons "\\.ils$"  'lisp-mode)
                    (cons "\\.vh$"   'verilog-mode)
                    (cons "\\.vams$" 'verilog-mode)
                    (cons "\\.vo$"   'verilog-mode)
                    (cons "\\.vn$"   'verilog-mode)
                    (cons "\\.svh$"  'verilog-mode)
                    (cons "\\.xdc$"  'tcl-mode)
                    (cons "\\.cpf$"  'tcl-mode)
                    (cons "\\.sdc$"  'tcl-mode)
                    (cons "\\.tm$"   'tcl-mode)
                    (cons "\\.l$"    'c++-mode)
                    (cons "makefile.inc"  'makefile-gmake-mode))
              auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка окружения.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(c-electric-flag t t)
 '(c-offsets-alist
   '((member-init-intro . ++)
     (statement-cont . ++)
     (inher-intro . ++)
     (access-label . -1)
     (arglist-intro . ++)
     (topmost-intro-cont . ++)
     (innamespace . 0)))
 '(column-number-mode t)
 '(default-frame-alist
    '((menu-bar-lines . 0)
      (top . 0)
      (left . 0)
      (width . 80)
      (height . 70)))
 '(default-tab-width 2)
 '(dired-listing-switches "-alnGho")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.[^.].+$")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   '((menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (top . 0)
     (left . 0)
     (width . 80)
     (height . 70)))
 '(package-selected-packages
   '(doxymacs use-package sr-speedbar yaml-mode lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode helm iedit flycheck-pos-tip evil docker magit markdown-mode org treemacs delsel))
 '(python-indent 2)
 '(ring-bell-function 'ignore)
 '(save-place-mode 1)
 '(tcl-continued-indent-level 4)
 '(tcl-indent-level 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(user-full-name "harkut")
 '(user-mail-address "yanovets.vasya@gmail.com")
 '(verilog-auto-newline nil)
 '(verilog-case-indent 2)
 '(verilog-cexp-indent 2)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-p1800-keywords t)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-directive 2)
 '(verilog-indent-level-module 2)
 '(verilog-minimum-comment-distance 5)
 '(warning-suppress-log-types '((initialization) (comp)))
 '(warning-suppress-types '((comp))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Рожа" Emacs-а.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#202022" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 70 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Более краткий emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

(use-package delsel
  :config (delete-selection-mode t)) ;; Удалять выделенный фрагмент при вводе текста
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Переход к конфигу.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key "\C-x/" 'load-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Буфер *scratch* не нужен.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun init-kill-scratch ()
;;   "Закрыть буфер *scratch* при запуске редактора или подключении клиента."
;;   (when (get-buffer "*scratch*")
;;     (kill-buffer "*scratch*")))
;; (add-hook 'after-init-hook 'init-kill-scratch)
;; (add-hook 'server-after-make-frame-hook 'init-kill-scratch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка пакета desktop.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package desktop
  :custom
  (desktop-dirname ".")
  (desktop-path (list "."))
  (desktop-load-locked-desktop 1)
  (desktop-restore-frames 1)
  (desktop-save 1)
  :config
  (desktop-save-mode 1)
  (add-to-list 'delete-frame-functions 'desktop-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  :hook
  (after-init . desktop-read)
  (server-after-make-frame . desktop-read)
  (kill-emacs . (lambda () (desktop-save "." 1)))
  (server-done . desktop-save)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка пакета treemacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :bind
  ("C-x t t"   . treemacs)
  ("M-0"       . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t B"   . treemacs-bookmark)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка пакета orgmode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :custom
  (org-log-done 'time)
  :mode
  ("\\.org$" . org-mode)
  :bind
  ("C-x C-t" . org-todo)
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка пакета valgrind.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun valgrind ()
;;   (interactive)
;;   (compilation-minor-mode)
;;   (define-key compilation-minor-mode-map (kbd "M-p") ‘comint-send-input)
;;     (define-key compilation-minor-mode-map (kbd "S-M-p") ‘compile-goto-error))

;; (add-hook ‘shell-mode-hook ‘valgrind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка пакета flycheck.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(setq-default flycheck-disabled-checkers '(c/c++-clang))
(global-flycheck-mode)
;Disable Flycheck for LaTex
(setq flycheck-global-modes '(not LaTeX-mode latex-mode))
;load flycheck settings from current directory file .emacs.flycheck.el
(add-to-list 'load-path ".")
(if (file-exists-p ".emacs.flycheck.el")
   (require '.emacs.flycheck) )

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Нормальный бег между буферами в окне
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Убить все буферы кроме выделенного
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun kill-other-buffers ()
;;     "Kill all other buffers."
;;     (interactive)
;;     (mapc 'kill-buffer 
;;           (delq (current-buffer) 
;;                 (remove-if-not 'buffer-file-name (buffer-list)))))

;; (defun close-all-buffers ()
;;   (interactive)
;;   (mapc 'kill-buffer (buffer-list)))

;; (defun only-current-buffer () 
;;   (interactive)
;;     (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настраиваем пакет lsp и helm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
             :hook ((c++-mode c-mode glsl-mode yaml-mode) . lsp-deferred)
             :commands lsp lsp-deferred)

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))

(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настраиваем пакет gdb.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'exec-path "/usr/bin/gdb")
(use-package gdb
  :custom
  (gdb-many-windows t)
  (dap-auto-configure-mode nil)
  :bind
  ("<f5>" . gud-cont)
  ("<f6>" . gud-next)
  )
;; ;;(require 'gdb) ; Загружает GDB-режим
;; (setq gdb-many-windows t) ; Открывает несколько окон (stdout, regs, etc.)
;; (setq dap-auto-configure-mode nil)

;; (defun my-gdb-mode-hook ()
;;   "Customizations for gdb-mode."
;;   ;; Bind F5 to 'gud-cont' (continue)
;;   (local-set-key (kbd "<f5>") 'gud-cont)
;;   ;; Bind F10 to 'gud-next' (step over)
;;   (local-set-key (kbd "<f10>") 'gud-next))

;; (add-hook 'gdb-mode-hook 'my-gdb-mode-hook)

;; ;; Автоматическая загрузка вашего .gdbinit
;; (if (file-exists-p "~/.gdbinit")
;;     (load-file "~/.gdbinit")
;;   (message "GDB init file not found: ~/.gdbinit"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настраиваем пакет doxygen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doxymacs
  :hook
  (c-mode-common-hook . doxymacs-mode)
  :mode
  ("Doxyfile" . doxymacs-mode)
  ("\\.c$" . doxymacs-mode)
  :bind 
  ;; Lookup documentation for the symbol at point.
  ("C-c d ?" . doxymacs-lookup)
  ;; Rescan your Doxygen tags file.
  ("C-c d r" . doxymacs-rescan-tags)
  ;; Prompt you for a Doxygen command to enter, and its
  ;; arguments.
  ("C-c d RET" . doxymacs-insert-command)
  ;; Insert a Doxygen comment for the next function.
  ("C-c d f" . doxymacs-insert-function-comment)
  ;; Insert a Doxygen comment for the current file.
  ("C-c d i" . doxymacs-insert-file-comment)
  ;; Insert a Doxygen comment for the current member.
  ("C-c d ;" . doxymacs-insert-member-comment)
  ;; Insert a blank multi-line Doxygen comment.
  ("C-c d m" . doxymacs-insert-blank-multiline-comment)
  ;; Insert a blank single-line Doxygen comment.
  ("C-c d s" . doxymacs-insert-blank-singleline-comment)
  ;; Insert a grouping comments around the current region.
  ("C-c d @" . doxymacs-insert-grouping-comments)
  ;; :custom
  ;;   (doxymacs-doxygen-dirs
  ;;    '(("^/home/me/project/foo/"
  ;;       "http://someplace.com/doc/foo/foo.xml"
  ;;        "http://someplace.com/doc/foo/")
  ;;      ("^/home/me/project/bar/"
  ;;       "~/project/bar/doc/bar.xml"
  ;;       "file:///home/me/project/bar/doc/")))
)

