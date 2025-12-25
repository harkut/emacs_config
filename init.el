;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Система пакетов
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Helm (Интерфейс и Навигация)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :init
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-s" . helm-occur)
   ("M-y" . helm-show-kill-ring)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   :map helm-find-files-map
   ("C-l" . helm-find-files-up-one-level))
  :custom
  (helm-ff-auto-update-initial-value nil)
  (helm-find-files-ignore-thing-at-point t)
  (helm-split-window-inside-p nil)             ; Открывать на всю ширину
  (helm-echo-input-in-header-line nil)
  (helm-display-header-line nil)
  :config
  ;; ИСПРАВЛЕНИЕ: Используем специальную функцию, которая умеет обходить
  ;; выделенные (dedicated) окна window-purpose
  (setq helm-display-function 'helm-display-buffer-in-own-frame)

  ;; Если вы хотите, чтобы Helm открывался в том же фрейме, но игнорировал блокировку:
  (setq helm-display-function (lambda (buffer &optional resume)
                                (let ((display-buffer-overriding-action
                                       '(nil . ((inhibit-same-window . nil)
                                                (inhibit-switch-frame . nil)))))
                                  (helm-default-display-buffer buffer resume))))

  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Ядро Emacs и Интерфейс
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :custom
  (user-full-name "harkut")
  (user-mail-address "yanovets.vasya@gmail.com")
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (column-number-mode t)
  (indent-tabs-mode nil)
  (default-tab-width 2)
  (save-place-mode 1)
  (fill-column 80)
  (display-fill-column-indicator-column 80)
  (initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 70)))
  (default-frame-alist '((top . 0) (left . 0) (width . 80) (height . 70)))
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)

  (global-visual-line-mode -1)
  (global-display-fill-column-indicator-mode 1)

  ;; Убираем ограничитель в отладчике и консолях
  (add-hook 'gud-mode-hook (lambda ()
                             (display-fill-column-indicator-mode -1)))
  (add-hook 'comint-mode-hook (lambda ()
                                (display-fill-column-indicator-mode -1)))

  ;; Умная функция переноса для защиты синтаксиса
  (defun my/setup-smart-wrap ()
    "Настройка: в коде только в комментариях, в тексте — везде."
    (setq fill-column 80)
    (if (derived-mode-p 'prog-mode 'makefile-mode)
        (setq comment-auto-fill-only-comments t)
      (setq comment-auto-fill-only-comments nil))
    (auto-fill-mode 1))

  ;; Применяем ко всем режимам (C, Lisp, Makefile, Org)
  (add-hook 'prog-mode-hook 'my/setup-smart-wrap)
  (add-hook 'text-mode-hook 'my/setup-smart-wrap)
  (add-hook 'makefile-mode-hook 'my/setup-smart-wrap)
  (add-hook 'lisp-mode-hook 'my/setup-smart-wrap)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (custom-set-faces
   '(default ((t (:stipple nil :background "#202022" :foreground "white"
                           :height 70 :family "DejaVu Sans Mono"))))))

;; Восстановление сессии (открытые файлы, курсоры)
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode 1)
  (setq desktop-dirname ".")           ; Сохранять десктоп в папке запуска
  (setq desktop-path '("."))           ; Искать десктоп только в текущей папке
  (setq desktop-save t)                ; Сохранять автоматически без вопросов
  (setq desktop-load-locked-desktop t) ; Грузить, даже если есть lock-файл
  (setq desktop-restore-eager 10))     ; Восстанавливать первые 10 буферов сразу

;; Перемещение между окнами M + стрелки
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings 'M))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Кодировки (Windows-1251)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mule
  :ensure nil
  :config
  (set-language-info-alist
   "Windows-1251" `((charset cp1251) (coding-system cp1251)
                    (coding-priority cp1251)
                    (input-method . "russian-typewriter")
                    (features cyril-util)
                    (sample-text . "Russian (Русский) Здравствуйте!")
                    (documentation . "Support for Windows cp1251."))
   '("Cyrillic"))
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Ассоциации файлов (auto-mode-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append '(;; Spice Mode
                ("\\.sp$"    . spice-mode)
                ("\\.cir$"   . spice-mode)
                ("\\.ckt$"   . spice-mode)
                ("\\.mod$"   . spice-mode)
                ("\\.cdl$"   . spice-mode)
                ("\\.chi$"   . spice-mode)
                ("\\.inp$"   . spice-mode)
                ("\\.scs$"   . spice-mode)
                ;; Assembler
                ("\\.a51$"   . asm-mode)
                ;; C / C++
                ("\\.cc$"    . c++-mode)
                ("\\.cpp$"   . c++-mode)
                ("\\.cxx$"   . c++-mode)
                ("\\.h$"     . c++-mode)
                ("\\.tcc$"   . c++-mode)
                ("\\.icc$"   . c++-mode)
                ("\\.l$"     . c++-mode)
                ("\\.c$"     . c-mode)
                ;; Lisp
                ("\\.il$"    . lisp-mode)
                ("\\.ils$"   . lisp-mode)
                ;; Verilog
                ("\\.vh$"    . verilog-mode)
                ("\\.vams$"  . verilog-mode)
                ("\\.vo$"    . verilog-mode)
                ("\\.vn$"    . verilog-mode)
                ("\\.svh$"   . verilog-mode)
                ;; Tcl
                ("\\.xdc$"   . tcl-mode)
                ("\\.cpf$"   . tcl-mode)
                ("\\.sdc$"   . tcl-mode)
                ("\\.tm$"    . tcl-mode)
                ;; Docker
                ("Dockerfile\\'" . dockerfile-mode)
                ("docker-compose\\.yml\\'" . yaml-mode)
                ;; Makefile
                ("\\.mk\\'" . makefile-gmake-mode)
                ("makefile\\.inc\\'" . makefile-gmake-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Интеграция Valgrind (Интерактивный запуск)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package compile
  :ensure nil
  :bind ("<f7>" . valgrind)
  :preface
  (defgroup valgrind nil
    "Run valgrind as inferior of Emacs, parse error messages."
    :group 'tools
    :group 'processes)

  (defcustom valgrind-command "valgrind --leak-check=full "
    "Last shell command used to run valgrind."
    :type 'string
    :group 'valgrind)

  (defvar valgrind-history nil)

  (defun valgrind (command)
    "Запуск Valgrind интерактивно с переходом в конец буфера."
    (interactive
     (if (or compilation-read-command current-prefix-arg)
         (list (read-from-minibuffer "Valgrind command: "
                                     (if (stringp valgrind-command)
                                         valgrind-command
                                       (eval valgrind-command))
                                     nil nil '(valgrind-history . 1)))
       (list (if (stringp valgrind-command) valgrind-command
               (eval valgrind-command)))))
    (unless (equal command valgrind-command)
      (setq valgrind-command command))

    (let* ((buffer-name "*valgrind*")
           (buffer (get-buffer-create buffer-name))
           (cmd-args (split-string-and-unquote command)))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (apply 'make-comint-in-buffer "valgrind" buffer
               (car cmd-args) nil (cdr cmd-args))
        (compilation-shell-minor-mode 1)
        (setq-local comint-scroll-show-maximum-output t)
        (setq-local comint-output-filter-functions
                    (append comint-output-filter-functions
                            '(comint-postoutput-scroll-to-bottom))))
      (pop-to-buffer buffer)
      (goto-char (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Языки и Отступы
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-offset 2)
  (c-electric-flag t)
  :config
  (setq c-offsets-alist
        '((member-init-intro . ++) (statement-cont . ++) (inher-intro . ++)
          (access-label . -1) (arglist-intro . ++) (topmost-intro-cont . ++)
          (innamespace . 0)))
  :hook
  (c-mode-common . (lambda ()
                     (hs-minor-mode t)
                     (local-set-key (kbd "C-c <right>") 'hs-show-block)
                     (local-set-key (kbd "C-c <left>")  'hs-hide-block))))

(use-package verilog-mode
  :custom
  (verilog-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-behavioral 2)
  (verilog-case-indent 2)
  (verilog-auto-newline nil)
  (verilog-highlight-p1800-keywords t))

(use-package tcl
  :ensure nil
  :custom
  (tcl-indent-level 2)
  (tcl-continued-indent-level 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Инструменты (GDB, Doxymacs, LSP, Flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gdb-mi
  :ensure nil
  :init (add-to-list 'exec-path "/usr/bin/")
  :custom
  (gdb-many-windows nil)
  :bind
  ("<f5>" . gud-cont)
  ("<f6>" . gud-next)
  :config
  (advice-add 'gdb-input :filter-args
              (lambda (args)
                (let ((str (car args)))
                  (if (and (stringp str) (string-match
                                          "set target-async" str))
                      (list (replace-regexp-in-string
                             "set target-async" "set mi-async" str) (cadr args))
                    args)))
              '((name . "fix-gdb-deprecated-async"))))

(use-package doxymacs
  :hook (c-mode-common . doxymacs-mode)
  :mode ("Doxyfile" . doxymacs-mode)
  :bind
  (("C-c d ?" . doxymacs-lookup)
   ("C-c d r" . doxymacs-rescan-tags)
   ("C-c d RET" . doxymacs-insert-command)
   ("C-c d f" . doxymacs-insert-function-comment)
   ("C-c d i" . doxymacs-insert-file-comment)
   ("C-c d ;" . doxymacs-insert-member-comment)
   ("C-c d m" . doxymacs-insert-blank-multiline-comment)
   ("C-c d s" . doxymacs-insert-blank-singleline-comment)
   ("C-c d @" . doxymacs-insert-grouping-comments)))
:config
:config
(require 'project)

  (defun my/get-project-root-git ()
    "Находит корень проекта, ориентируясь на .git."
    (let ((pr (project-current)))
      (cond (pr (project-root pr))
            ((locate-dominating-file default-directory ".git"))
            (t nil))))

  (defun my/doxygen-generate-from-root ()
    "Запускает 'make doc' в корневом каталоге проекта."
    (interactive)
    (let ((root (my/get-project-root-git)))
      (if (and root (file-exists-p (expand-file-name "Makefile" root)))
          (let ((default-directory root))
            (message "Doxygen: запуск сборки из корня: %s" root)
            (start-process "doxygen-root-make" "*doxygen-log*" "make" "doc"))
        (message "Makefile не найден в корне %s" (or root "не определен")))))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (add-hook 'after-save-hook
                        #'my/doxygen-generate-from-root nil t)))

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(c/c++-clang))
  :config
  (when (file-exists-p ".emacs.flycheck.el") (load ".emacs.flycheck.el")))

(use-package lsp-mode
  :hook ((c++-mode c-mode yaml-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Org Mode (Заметки и Задачи в иерархии проекта)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure nil
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :preface
  (declare-function org-save-all-org-buffers "org")
  (defun my/org-save-all-before-agenda (&rest _)
    (interactive)
    (when (featurep 'org) (org-save-all-org-buffers)))

  (defun my/org-current-project-notes ()
    "Ищет Makefile и возвращает путь к notes.org."
    (let ((project-dir (locate-dominating-file default-directory "Makefile")))
      (if project-dir
          (expand-file-name "notes.org" project-dir)
        (expand-file-name "notes.org" "~/org/"))))

  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org"))
  (org-capture-templates
   '(("t" "Задачка (Глобальная)" entry
      (file+headline "~/org/tasks.org" "Входящие")
      "* TODO %?\n  Создано: %U\n  %a")
     ("n" "Заметка к проекту (ex_*)" entry
      (file+headline my/org-current-project-notes "Notes")
      "* %?\n  Дата: %U\n  Контекст: %a\n  %i")))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-hide-leading-stars t)
  (org-ellipsis " ▼")
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  ;; Включаем физический перенос и в Org
  (add-hook 'org-mode-hook 'my/force-auto-fill)
  (with-eval-after-load 'org-agenda
    (advice-add 'org-agenda :before #'my/org-save-all-before-agenda)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Навигация и Дополнительно (GDB IDE - Custom Proportions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'gdb-mi)

(use-package delsel
  :ensure nil
  :config (delete-selection-mode t))

(setq gdb-many-windows nil)

(defun my/create-4-pane-grid ()
  "Создает сетку 2x2 в текущем фрейме."
  (delete-other-windows)
  (let* ((w1 (selected-window))
         (w2 (split-window w1 nil 'right))
         (w3 (split-window w1 nil 'below))
         (w4 (split-window w2 nil 'below)))
    (dolist (w (list w1 w2 w3 w4))
      (with-selected-window w
        (purpose-set-window-purpose 'edit)))))

(defun my/get-gdb-buffer-by-mode (mode)
  "Поиск буфера GDB по Major Mode."
  (cl-find-if (lambda (b) (with-current-buffer b (eq major-mode mode)))
              (buffer-list)))

(defun my/populate-gdb-windows ()
  "Заполнение окон строго по их назначению."
  (interactive)
  (let ((mode-to-purpose '((gdb-locals-mode       . gdb-locals)
                           (gdb-registers-mode    . gdb-regs)
                           (gdb-stack-buffer-mode  . gdb-stack)
                           (gdb-frames-mode       . gdb-stack)
                           (gdb-breakpoints-mode  . gdb-breaks)
                           (gdb-disassembly-mode  . gdb-disasm)
                           (gdb-inferior-io-mode  . gdb-io)
                           (gud-mode              . gdb-out))))
    (dolist (mapping mode-to-purpose)
      (let* ((mode (car mapping))
             (purp (cdr mapping))
             (buf (my/get-gdb-buffer-by-mode mode))
             (win (cl-find-if (lambda (w)
                                (and (fboundp 'purpose-window-purpose-get)
                                     (eq (purpose-window-purpose-get w) purp)))
                              (window-list))))
        (when (and buf win)
          (set-window-buffer win buf)
          (set-window-dedicated-p win t))))))

(defun my/apply-gdb-layout ()
  "Построение IDE во втором фрейме с пропорциями 10-10-10-10-60 справа."
  (interactive)
  (delete-other-windows)
  (let* ((left-win (selected-window))
         (right-win
          (split-window left-win (floor (* (window-total-width) 0.55)) 'right)))

    ;; ЛЕВАЯ КОЛОННА
    (with-selected-window left-win
      (let* ((w-gdb (split-window-below (floor (* (window-height) 0.7))))
             (w-io  (with-selected-window w-gdb
                      (split-window-below (floor (* (window-height) 0.5))))))
        (purpose-set-window-purpose 'edit left-win)
        (purpose-set-window-purpose 'gdb-out w-gdb)
        (purpose-set-window-purpose 'gdb-io w-io)
        (switch-to-buffer (other-buffer (current-buffer) t))))

    ;; ПРАВАЯ КОЛОННА (Настройка пропорций)
    (with-selected-window right-win
      (let* ((total-h (window-height))
             (h-step (floor (* total-h 0.1))) ; Шаг в 10%
             (w-stack  (split-window-below h-step))
             (w-locals (with-selected-window w-stack (split-window-below h-step)))
             (w-regs   (with-selected-window w-locals (split-window-below h-step)))
             (w-breaks (with-selected-window w-regs (split-window-below h-step))))

        (purpose-set-window-purpose 'gdb-disasm right-win)
        (purpose-set-window-purpose 'gdb-stack  w-stack)
        (purpose-set-window-purpose 'gdb-locals w-locals)
        (purpose-set-window-purpose 'gdb-regs   w-regs)
        (purpose-set-window-purpose 'gdb-breaks w-breaks)))

    ;; Генерация буферов
    (gdb-display-io-buffer)
    (gdb-display-gdb-buffer)
    (gdb-display-registers-buffer)
    (gdb-display-locals-buffer)
    (gdb-display-stack-buffer)
    (gdb-display-breakpoints-buffer)
    (gdb-display-disassembly-buffer)

    (run-with-timer 1.0 nil #'my/populate-gdb-windows)))

(use-package window-purpose
  :config
  (purpose-mode 1)
  ;; Разрешаем Helm открываться поверх Purpose-окон
  (add-to-list 'purpose-user-regexp-purposes '("^\\*helm" . nil))

  (defun load-purpose-mode ()
    (interactive)
    ;; Прижимаем основной фрейм влево
    (set-frame-parameter nil 'left 0)
    (set-frame-parameter nil 'top 0)
    (my/create-4-pane-grid)

    (let ((f2 (make-frame '((name . "GDB-DEBUG")
                            (width . 200)
                            (height . 85)
                            (left . -1)  ; Прижимаем вправо
                            (top . 0)))))
      (with-selected-frame f2
        (call-interactively 'gdb)
        (my/apply-gdb-layout))))
  :bind (("M-L" . load-purpose-mode)
         ("M-g L" . my/apply-gdb-layout)))

(global-set-key (kbd "C-x /") (lambda () (interactive)
                                (find-file "~/.emacs.d/init.el")))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(window-purpose org-bullets yasnippet yaml-mode which-key vterm use-package sr-speedbar projectile popup pkg-info org magit iedit helm-xref helm-lsp haskell-mode flycheck-pos-tip evil doxymacs docker dap-mode company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#202022" :foreground "white" :height 70 :family "DejaVu Sans Mono")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Git (Magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12. Docker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package docker
  :bind ("C-c M-d" . docker)
  :custom
  (docker-command "docker"))

(use-package dockerfile-mode)
