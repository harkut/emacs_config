;; init.el

;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0. Безопасность и производительность
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error t)
(message "Начинаем загрузку конфигурации...")
(require 'make-mode)

;; Ускорение package.el: отключаем проверки и обновления при старте
(setq package-check-signature nil
      package-archive-check-signature nil
      package-enable-at-startup nil      ; Не загружать пакеты автоматически
      package-update-contents-and-compile nil) ; Не компилировать при обновлении

;; Блокировать проверку архивов при каждом запуске
(defvar my/package-archives-checked nil)
(unless my/package-archives-checked
  (setq my/package-archives-checked t)
  (package-refresh-contents t)) ; t = только если кэш устарел

;; Ускорение use-package
(setq use-package-enable-imenu-support t
      use-package-verbose nil           ; Отключить вывод отладки
      use-package-minimum-reported-time 0.1) ; Показывать только медленные пакеты

;; ;; Ускорение file-name-handler-alist (кэширование путей)
;; (defvar my/file-name-handler-alist-original file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq file-name-handler-alist my/file-name-handler-alist-original)))

;; Отдельный файл для настроек GUI
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1 Система ручной установки пакетов с авто-загрузкой
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Основной путь для ручных пакетов
(defconst my-manual-packages-dir
  (expand-file-name "manual-packages" user-emacs-directory)
  "Директория для пакетов, установленных вручную.")

;; Создаем директорию, если ее нет
(unless (file-exists-p my-manual-packages-dir)
  (make-directory my-manual-packages-dir t))

;; Функция для добавления всех поддиректорий в load-path
(defun add-subdirs-to-load-path (parent-dir)
  "Добавляет все поддиректории PARENT-DIR в load-path."
  (let ((default-directory parent-dir))
    (when (file-directory-p parent-dir)
      (setq load-path
            (append
             (mapcar 'expand-file-name
                     (cl-remove-if-not
                      'file-directory-p
                      (directory-files parent-dir t "^[^.]")))
             load-path)))))

;; Добавляем все поддиректории manual-packages в load-path
(add-subdirs-to-load-path my-manual-packages-dir)

;; Проверка доступности git
(defun my/git-available-p ()
  "Проверяет, доступен ли git для клонирования репозиториев."
  (executable-find "git"))

;; Исправленная функция преобразования URL
(defun my/github-url-to-git (url)
  "Преобразует URL GitHub в формат для git clone."
  (cond
   ((string-match-p "\\.git$" url) url)   ; уже заканчивается на .git
   ((string-match "github\\.com" url) (concat url ".git"))
   (t url)))

;; Проверка, правильно ли распакован пакет
(defun my/check-package-unpacked (pkg-dir)
  "Проверяет, правильно ли распакован пакет в PKG-DIR."
  (when (file-directory-p pkg-dir)
    (let ((files (directory-files pkg-dir t "^[^.]")))
      (or (cl-find-if (lambda (f) (string-match "\\.el$" (file-name-nondirectory f))) files)
          (cl-find-if (lambda (f) (file-directory-p f)) files)))))

;; Автоматическая загрузка пакета с GitHub с исправленным URL
(defun my/download-package-from-github (pkg-name github-url)
  "Загружает пакет с GitHub в manual-packages."
  (let* ((pkg-str (symbol-name pkg-name))
         (target-dir (expand-file-name pkg-str my-manual-packages-dir))
         (git-url (my/github-url-to-git github-url)))

    ;; Если git не доступен, пропускаем
    (unless (my/git-available-p)
      (message "Git не найден! Не могу загрузить пакет %s" pkg-str)
      (return-from my/download-package-from-github nil))

    ;; Если директория уже существует, пропускаем
    (when (file-exists-p target-dir)
      (message "Пакет %s уже существует в manual-packages" pkg-str)
      (return-from my/download-package-from-github t))

    ;; Пытаемся клонировать репозиторий
    (message "Загрузка пакета %s с GitHub..." pkg-str)
    (let* ((default-directory my-manual-packages-dir)
           (command (format "git clone --depth 1 --quiet %s %s" git-url pkg-str))
           (result (shell-command command)))

      (if (zerop result)
          (progn
            (message "Пакет %s успешно загружен" pkg-str)
            ;; Проверяем, что пакет правильно распакован
            (when (my/check-package-unpacked target-dir)
              ;; Добавляем директорию в load-path
              (add-to-list 'load-path target-dir)
              t)
            t)
        (message "Ошибка загрузки пакета %s: код возврата %d" pkg-str result)
        nil))))

;; Улучшенная функция загрузки пакета с GitHub (без git clone)
(defun my/download-package-direct (pkg-name github-url)
  "Загружает пакет напрямую с GitHub (без git clone)."
  (let* ((pkg-str (symbol-name pkg-name))
         (target-dir (expand-file-name pkg-str my-manual-packages-dir))
         (archive-url (concat github-url "/archive/main.zip"))
         (temp-file (expand-file-name (format "%s-main.zip" pkg-str) temporary-file-directory)))

    (message "Прямая загрузка пакета %s..." pkg-str)

    (condition-case err
        (progn
          ;; Скачиваем архив
          (url-copy-file archive-url temp-file t)

          ;; Создаем директорию
          (make-directory target-dir t)

          ;; Распаковываем архив
          (let ((default-directory target-dir))
            (shell-command (format "unzip -q -o %s" temp-file)))

          ;; Удаляем временный файл
          (delete-file temp-file)

          ;; Перемещаем файлы из поддиректории
          (let ((subdir (expand-file-name (format "%s-main" pkg-str) target-dir)))
            (when (file-directory-p subdir)
              (dolist (file (directory-files subdir t "^[^.]"))
                (unless (member (file-name-nondirectory file) '("." ".."))
                  (rename-file file target-dir t)))
              (delete-directory subdir)))

          ;; Проверяем наличие .el файлов
          (if (my/check-package-unpacked target-dir)
              (progn
                (message "Пакет %s успешно загружен напрямую" pkg-str)
                (add-to-list 'load-path target-dir)
                t)
            (progn
              (message "Пакет %s загружен, но не содержит .el файлов" pkg-str)
              nil))
          t)
      (error
       ;; Пробуем скачать с ветки master
       (condition-case err2
           (progn
             (setq archive-url (concat github-url "/archive/master.zip"))
             (setq temp-file (expand-file-name (format "%s-master.zip" pkg-str) temporary-file-directory))

             (url-copy-file archive-url temp-file t)
             (make-directory target-dir t)

             (let ((default-directory target-dir))
               (shell-command (format "unzip -q -o %s" temp-file)))

             (delete-file temp-file)

             (let ((subdir (expand-file-name (format "%s-master" pkg-str) target-dir)))
               (when (file-directory-p subdir)
                 (dolist (file (directory-files subdir t "^[^.]"))
                   (unless (member (file-name-nondirectory file) '("." ".."))
                     (rename-file file target-dir t)))
                 (delete-directory subdir)))

             (if (my/check-package-unpacked target-dir)
                 (progn
                   (message "Пакет %s успешно загружен напрямую (master)" pkg-str)
                   (add-to-list 'load-path target-dir)
                   t)
               (progn
                 (message "Пакет %s загружен, но не содержит .el файлов" pkg-str)
                 nil)))
         (error
          (message "Ошибка прямой загрузки %s: %s" pkg-str err2)
          nil))))))

;; Универсальная функция загрузки пакета с улучшенной обработкой ошибок
(defun my/ensure-package (pkg-name &optional github-url custom-load-file)
  "Универсальная функция загрузки пакета с авто-загрузкой с GitHub."

  (let* ((pkg-symbol pkg-name)
         (pkg-str (symbol-name pkg-name))
         (pkg-dir (expand-file-name pkg-str my-manual-packages-dir))
         (load-file (or custom-load-file (concat pkg-str ".el")))
         (manual-path (expand-file-name load-file pkg-dir))
         (success nil))
    (unless (symbolp pkg-name)
      (setq pkg-symbol (intern (symbol-name pkg-name))))
    ;; 1. Пытаемся установить через package.el (если доступны репозитории)
    (unless (package-installed-p pkg-symbol)
      (condition-case err
          (progn
            (package-refresh-contents)
            (package-install pkg-symbol)
            (message "Пакет %s установлен через package.el" pkg-str)
            (setq success t))
        (error
         (message "Не удалось установить %s через package.el" pkg-str))))

    ;; 2. Если пакет не установлен, пробуем загрузить из manual-packages
    (unless (or success (package-installed-p pkg-symbol) (featurep pkg-symbol))

      ;; Если пакета нет в manual-packages, пробуем скачать
      (unless (file-exists-p manual-path)
        (when github-url
          ;; Сначала пробуем через git clone
          (if (my/git-available-p)
              (setq success (my/download-package-from-github pkg-name github-url))
            ;; Если git не доступен, пробуем прямую загрузку
            (setq success (my/download-package-direct pkg-name github-url)))))

      ;; Теперь проверяем снова
      (when (or (file-exists-p manual-path) success)
        ;; Добавляем директорию пакета в load-path
        (unless (member pkg-dir load-path)
          (add-to-list 'load-path pkg-dir))

        ;; Пытаемся загрузить пакет
        (condition-case err
            (progn
              (require pkg-symbol nil 'noerror)
              (when (featurep pkg-symbol)
                (message "Пакет %s загружен из manual-packages" pkg-str)
                (setq success t)))
          (error
           (message "Ошибка загрузки %s: %s" pkg-str err)))))

    ;; 3. Если пакет все еще не загружен, показываем инструкцию
    (unless (or success (package-installed-p pkg-symbol) (featurep pkg-symbol))

      ;; Если есть URL, предлагаем загрузить вручную
      (when github-url
        (message (concat "\nПакет %s не найден!\n"
                         "Для автоматической загрузки убедитесь, что git установлен.\n"
                         "Или установите вручную:\n"
                         "1. Скачайте с GitHub: %s\n"
                         "2. Распакуйте в: %s\n"
                         "3. Перезапустите Emacs")
                 pkg-str github-url pkg-dir)))

    ;; 4. Возвращаем успешность загрузки
    (or success (package-installed-p pkg-symbol) (featurep pkg-symbol))))

;; Функция для массовой загрузки пакетов с авто-загрузкой
(defun my/ensure-packages (package-list)
  "Загружает несколько пакетов из списка с авто-загрузкой."
  (dolist (pkg-spec package-list)
    (let ((pkg-name (car pkg-spec))
          (github-url (cadr pkg-spec))
          (load-file (caddr pkg-spec)))
      (my/ensure-package pkg-name github-url load-file))))

;; Функция для принудительного обновления всех пакетов
(defun my/update-all-packages ()
  "Обновляет все пакеты из manual-packages."
  (interactive)
  (unless (my/git-available-p)
    (error "Git не найден! Установите git для обновления пакетов."))

  (let ((default-directory my-manual-packages-dir)
        (updated 0))
    (dolist (dir (directory-files my-manual-packages-dir t "^[^.]"))
      (when (file-directory-p dir)
        (let ((default-directory dir))
          (when (file-exists-p ".git")
            (message "Обновление %s..." (file-name-nondirectory dir))
            (when (zerop (shell-command "git pull --rebase --quiet"))
              (setq updated (1+ updated)))))))
    (message "Обновлено %d пакетов" updated)))

;; Функция для загрузки отсутствующих пакетов
(defun my/download-missing-packages ()
  "Загружает все отсутствующие пакеты из my/core-packages."
  (interactive)
  (let ((missing-count 0))
    (dolist (pkg-spec my/core-packages)
      (let ((pkg-name (car pkg-spec))
            (github-url (cadr pkg-spec))
            (pkg-dir (expand-file-name (symbol-name (car pkg-spec))
                                       my-manual-packages-dir)))
        (unless (file-exists-p pkg-dir)
          (if (my/git-available-p)
              (when (my/download-package-from-github pkg-name github-url)
                (setq missing-count (1+ missing-count)))
            (when (my/download-package-direct pkg-name github-url)
              (setq missing-count (1+ missing-count)))))))
    (message "Загружено %d отсутствующих пакетов" missing-count)))

;; Функция для очистки папки manual-packages
(defun my/clean-manual-packages (&optional force)
  "Удаляет все пакеты из manual-packages.
При FORCE не спрашивает подтверждения."
  (interactive "P")
  (let ((confirmed (or force
                       (yes-or-no-p "Удалить ВСЕ пакеты из manual-packages?")
                       (yes-or-no-p "Вы уверены? Это действие необратимо!"))))
    (when confirmed
      (let ((count 0)
            (default-directory my-manual-packages-dir))
        (dolist (dir (directory-files my-manual-packages-dir t "^[^.]"))
          (when (file-directory-p dir)
            (condition-case err
                (progn
                  (delete-directory dir t)
                  (setq count (1+ count)))
              (error
               (message "Ошибка удаления %s: %s" dir err)))))
        (message "Удалено %d пакетов из manual-packages" count)))))

;; Функция для исправления структуры пакета
(defun my/fix-package-structure (pkg-name)
  "Исправляет структуру пакета, если она неправильная."
  (interactive "SPackage name: ")
  (let* ((pkg-str (symbol-name pkg-name))
         (pkg-dir (expand-file-name pkg-str my-manual-packages-dir))
         (el-files (directory-files pkg-dir t "\\.el$")))

    (when (file-directory-p pkg-dir)
      ;; Если есть поддиректория с именем пакета, перемещаем файлы
      (let ((subdir (expand-file-name pkg-str pkg-dir)))
        (when (and (file-directory-p subdir)
                   (not (equal pkg-dir subdir)))
          (message "Исправление структуры пакета %s..." pkg-str)
          (dolist (file (directory-files subdir t "^[^.]"))
            (unless (member (file-name-nondirectory file) '("." ".."))
              (rename-file file pkg-dir t)))
          (delete-directory subdir)
          (message "Структура пакета %s исправлена" pkg-str))))))

;; Список пакетов для загрузки
(setq my/core-packages
      '((use-package "https://github.com/jwiegley/use-package")
        (helm "https://github.com/emacs-helm/helm")
        (which-key "https://github.com/justbur/emacs-which-key")
        (modern-cpp-font-lock "https://github.com/ludwigpacifici/modern-cpp-font-lock")
        (verilog-mode "https://github.com/veripool/verilog-mode")
        (yaml-mode "https://github.com/yoshiki/yaml-mode")
        (doxymacs "https://github.com/pniedzielski/doxymacs")
        (flycheck "https://github.com/flycheck/flycheck")
        (lsp-mode "https://github.com/emacs-lsp/lsp-mode")
        (company "https://github.com/company-mode/company-mode")
        (org-bullets "https://github.com/sabof/org-bullets")
        (window-purpose "https://github.com/bmag/emacs-purpose")
        (magit "https://github.com/magit/magit")
        (docker "https://github.com/Silex/docker.el")
        (dockerfile-mode "https://github.com/spotify/dockerfile-mode")
        (yasnippet "https://github.com/joaotavora/yasnippet")
        (yasnippet-snippets "https://github.com/AndreaCrotti/yasnippet-snippets")
        (wgrep "https://github.com/mhayashi1120/Emacs-wgrep")
        (ag "https://github.com/Wilfred/ag.el")
        (rg "https://github.com/dajva/rg.el")
        (pdf-tools "https://github.com/vedang/pdf-tools")
        (vterm "https://github.com/akermu/emacs-libvterm")
        ;; Пакеты для Lisp/SKILL разработки
        (slime "https://github.com/slime/slime")
        (slime-company "https://github.com/anwyn/slime-company")
        (highlight-parentheses "https://github.com/emacsmirror/highlight-parentheses")
        (rainbow-delimiters "https://github.com/Fanael/rainbow-delimiters")
        (paredit "https://github.com/emacsmirror/paredit")
        (lispy "https://github.com/abo-abo/lispy")
        (lisp-extra-font-lock "https://github.com/Lindydancer/lisp-extra-font-lock")
        (zoutline "https://github.com/abo-abo/zoutline")
        ;; Дополнительные полезные пакеты
        (projectile "https://github.com/bbatsov/projectile")
        (undo-tree "https://github.com/emacs-straight/undo-tree")
        (multiple-cursors "https://github.com/magnars/multiple-cursors.el")
        (expand-region "https://github.com/magnars/expand-region.el")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2 Определение глобальной карты команд
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Создаем глобальную карту команд для пользовательских функций
(defvar my-global-map (make-sparse-keymap)
  "Моя глобальная карта команд.")

;; Привязываем глобальную карту команд к C-c g
(global-set-key (kbd "C-c g") my-global-map) ;; g = Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Система пакетов (обновлено)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))) ; Резервный

;; Инициализация без спроса
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Загружаем все пакеты с авто-загрузкой
(my/ensure-packages my/core-packages)

;; Настраиваем use-package
(setq use-package-always-ensure nil
      use-package-verbose nil
      use-package-expand-minimally t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Helm (Интерфейс и Навигация)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :demand t  ; Helm всегда доступен
  :init
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
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
  (helm-split-window-inside-p nil)
  (helm-echo-input-in-header-line nil)
  (helm-display-header-line nil)
  (helm-autoresize-max-height 0)
  (helm-autoresize-min-height 20)
  ;; Исправленный display-function
  (helm-display-function (lambda (buffer &optional resume)
                           (let ((display-buffer-overriding-action
                                  '(display-buffer-same-window
                                    (inhibit-same-window . nil))))
                             (helm-default-display-buffer buffer resume))))
  :config
  ;; Автодополнение для минибуфера
  (helm-autoresize-mode 1))

;; Теперь определяем функции ПОСЛЕ загрузки helm
(defun my/ensure-helm-in-frame (frame)
  "Гарантирует, что helm-mode активен в указанном фрейме."
  (with-selected-frame frame
    (condition-case err
        (progn
          ;; Проверяем, загружен ли helm
          (unless (featurep 'helm)
            (require 'helm))
          ;; Включаем helm-mode, если он выключен
          (unless helm-mode
            (helm-mode 1))
          ;; Перепривязываем ключи для этого фрейма
          (global-set-key (kbd "M-x") 'helm-M-x)
          (global-set-key (kbd "C-x C-f") 'helm-find-files)
          (global-set-key (kbd "C-x b") 'helm-mini)
          (global-set-key (kbd "C-s") 'helm-occur)
          (global-set-key (kbd "M-y") 'helm-show-kill-ring))
      (error
       (message "Ошибка при включении helm во фрейме: %s" err)))))

;; Функция для принудительной активации which-key во всех фреймах
(defun my/ensure-which-key-in-frame (frame)
  "Гарантирует, что which-key-mode активен в указанном фрейме."
  (with-selected-frame frame
    (condition-case err
        (progn
          ;; Проверяем, загружен ли which-key
          (unless (featurep 'which-key)
            (require 'which-key))
          ;; Включаем which-key-mode, если он выключен
          (when (boundp 'which-key-mode)
            (unless which-key-mode
              (which-key-mode 1))))
      (error
       (message "Ошибка при включении which-key во фрейме: %s" err)))))

;; which-key для подсказок по клавишам
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
        which-key-popup-type 'minibuffer)

  ;; Добавляем хук для новых фреймов
  (add-hook 'after-make-frame-functions 'my/ensure-which-key-in-frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Ядро Emacs и Интерфейс
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  ;; Базовые настройки
  (setq user-full-name "harkut"
        user-mail-address "yanovets.vasya@gmail.com"
        inhibit-startup-screen t
        ring-bell-function 'ignore
        column-number-mode t
        indent-tabs-mode nil
        tab-width 2
        save-place-mode t
        fill-column 80
        display-fill-column-indicator-column 80
        initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 70))
        default-frame-alist '((top . 0) (left . 0) (width . 80) (height . 70)))

  :config
  ;; Упрощённые yes/no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Отключение GUI элементов
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)

  ;; Настройки отображения
  (global-visual-line-mode -1)
  (global-display-fill-column-indicator-mode 1)

  ;; Убираем ограничитель в специальных режимах
  (add-hook 'gud-mode-hook (lambda () (display-fill-column-indicator-mode -1)))
  (add-hook 'comint-mode-hook (lambda () (display-fill-column-indicator-mode -1)))
  (add-hook 'term-mode-hook (lambda () (display-fill-column-indicator-mode -1)))

  ;; Умный перенос текста
  (defun my/setup-smart-wrap ()
    "Настройка переноса: в коде только в комментариях, в тексте — везде."
    (setq-local fill-column 80)
    (if (derived-mode-p 'prog-mode 'makefile-mode)
        (setq-local comment-auto-fill-only-comments t)
      (setq-local comment-auto-fill-only-comments nil))
    (auto-fill-mode 1))

  ;; Применяем ко всем режимам
  (add-hook 'prog-mode-hook 'my/setup-smart-wrap)
  (add-hook 'text-mode-hook 'my/setup-smart-wrap)

  ;; Удаление пробелов в конце строк
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Режим подсветки парных скобок
  (show-paren-mode 1)
  (setq show-paren-delay 0))

;; Восстановление сессии (открытые файлы, курсоры)
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode 1)
  (setq desktop-dirname ".")           ; Сохранять десктоп в папке запуска
  (setq desktop-path '("."))           ; Искать десктоп только в текущей папке
  (setq desktop-save t)                ; Сохранять автоматически без вопросов
  (setq desktop-load-locked-desktop t) ; Грузить, даже если есть lock-файл
  (setq desktop-restore-eager 10)     ; Восстанавливать первые 10 буферов сразу
  (setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|\\.git/\\)"))

;; Перемещение между окнами
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings 'M))

;; Выделение заменяет текст
(use-package delsel
  :ensure nil
  :config (delete-selection-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Кодировки (Windows-1251)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mule
  :ensure nil
  :config
  (set-language-info-alist
   "Windows-1251" `((charset cp1251)
                    (coding-system cp1251)
                    (coding-priority cp1251)
                    (input-method . "russian-typewriter")
                    (features cyril-util)
                    (sample-text . "Russian (Русский) Здравствуйте!")
                    (documentation . "Support for Windows cp1251."))
   '("Cyrillic"))
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (setq-default buffer-file-coding-system 'utf-8-unix))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Ассоциации файлов (auto-mode-alist) - РАСШИРЕН ДЛЯ LISP/SKILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Функция для определения C/C++ заголовочных файлов
;; (defun my/c-or-c++-mode ()
;;   "Выбор между c-mode и c++-mode на основе содержимого файла."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (re-search-forward "^\\(.*\\)\\(.*\\)" nil t)
;;         (let ((content (match-string 0)))
;;           (if (or (string-match "class\\|namespace\\|template\\|cout\\|cin" content)
;;                   (string-match "\\.hpp\\|\\.hh\\|\\.hxx\\'" (buffer-file-name)))
;;               'c++-mode
;;             'c-mode))
;;       'c-mode)))

(setq auto-mode-alist
      (append '(;; Spice Mode
                ("\\.sp\\'"    . spice-mode)
                ("\\.cir\\'"   . spice-mode)
                ("\\.ckt\\'"   . spice-mode)
                ("\\.mod\\'"   . spice-mode)
                ("\\.cdl\\'"   . spice-mode)
                ("\\.chi\\'"   . spice-mode)
                ("\\.inp\\'"   . spice-mode)
                ("\\.scs\\'"   . spice-mode)
                ;; Assembler
                ("\\.a51\\'"   . asm-mode)
                ("\\.asm\\'"   . asm-mode)
                ("\\.s\\'"     . asm-mode)
                ;; C / C++
                ("\\.cc\\'"    . c++-mode)
                ("\\.cpp\\'"   . c++-mode)
                ("\\.cxx\\'"   . c++-mode)
                ("\\.hpp\\'"   . c++-mode)
                ("\\.hh\\'"    . c++-mode)
                ("\\.tcc\\'"   . c++-mode)
                ("\\.icc\\'"   . c++-mode)
                ("\\.c\\'"     . c-mode)
		            ("\\.h\\'"     . c-mode)
                ;; Flex/Lex файлы (отдельно!)
                ("\\.l\\'"     . c-mode)  ;; Предполагаем, что это C, если нет flex-mode
                ;; Lisp и SKILL файлы Cadence
                ("\\.il\\'"    . lisp-mode)
                ("\\.ils\\'"   . lisp-mode)
                ("\\.cdf\\'"   . lisp-mode)    ; Cadence Design Framework
                ("\\.bdf\\'"   . lisp-mode)    ; Cadence behavioral
                ("\\.lef\\'"   . lisp-mode)    ; Library Exchange Format
                ("\\.def\\'"   . lisp-mode)    ; Design Exchange Format
                ("\\.lisp\\'"  . lisp-mode)    ; Common Lisp
                ("\\.cl\\'"    . lisp-mode)    ; Common Lisp
                ("\\.asd\\'"   . lisp-mode)    ; ASDF system definition
                ("\\.lsp\\'"   . lisp-mode)    ; Lisp source
                ("\\.fasl\\'"  . lisp-mode)    ; Lisp compiled
                ("\\.sexp\\'"  . lisp-mode)    ; S-expressions
                ;; Verilog
                ("\\.v\\'"     . verilog-mode)
                ("\\.vh\\'"    . verilog-mode)
                ("\\.vams\\'"  . verilog-mode)
                ("\\.vo\\'"    . verilog-mode)
                ("\\.vn\\'"    . verilog-mode)
                ("\\.sv\\'"    . verilog-mode)
                ("\\.svh\\'"   . verilog-mode)
                ;; Tcl
                ("\\.tcl\\'"   . tcl-mode)
                ("\\.xdc\\'"   . tcl-mode)
                ("\\.cpf\\'"   . tcl-mode)
                ("\\.sdc\\'"   . tcl-mode)
                ("\\.tm\\'"    . tcl-mode)
                ;; Docker
                ("Dockerfile\\'" . dockerfile-mode)
                ("docker-compose\\.yml\\'" . yaml-mode)
                ("docker-compose\\.yaml\\'" . yaml-mode)
                ;; Makefile
                ("\\.mk\\'" . makefile-gmake-mode)
                ("Makefile\\.inc\\'" . makefile-gmake-mode)
                ("\\.make\\'" . makefile-gmake-mode)
                ;; YAML
                ("\\.yml\\'"  . yaml-mode)
                ("\\.yaml\\'" . yaml-mode)
                ;; Config файлы
                ("\\.cfg\\'"  . conf-mode)
                ("\\.conf\\'" . conf-mode)
                ("\\.ini\\'"  . conf-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Современная подсветка C++
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-cpp-font-lock-mode))

(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-offset 2)
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux")))
  (c-electric-flag t)
  :config
  (setq c-offsets-alist
        '((member-init-intro . ++)
          (statement-cont . ++)
          (inher-intro . ++)
          (access-label . -1)
          (arglist-intro . ++)
          (innamespace . 0)))
  :hook
  (c-mode-common . (lambda ()
                     (hs-minor-mode t)
                     (local-set-key (kbd "C-c <right>") 'hs-show-block)
                     (local-set-key (kbd "C-c <left>")  'hs-hide-block)
                     (local-set-key (kbd "C-c h") 'hs-toggle-hiding))))

(use-package verilog-mode
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.vh\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode))
  :custom
  (verilog-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-behavioral 2)
  (verilog-case-indent 2)
  (verilog-auto-newline nil)
  (verilog-auto-lineup 'all)
  (verilog-highlight-p1800-keywords t)
  (verilog-highlight-modules t)
  :hook (verilog-mode . (lambda ()
                          (setq electric-indent-mode nil))))

(use-package tcl
  :ensure nil
  :custom
  (tcl-indent-level 2)
  (tcl-continued-indent-level 4)
  (tcl-continued-indent-level 4))

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.5 Makefile Mode (окончательная рабочая версия)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Явно загружаем встроенный модуль make-mode
(require 'make-mode)

;; Устанавливаем ассоциации файлов
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("Makefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("Makefile\\.inc\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make\\'" . makefile-gmake-mode))

;; Базовая настройка без пользовательской подсветки
(defun my/makefile-simple-setup ()
  "Минимальная настройка для режима Makefile."
  (setq-local indent-tabs-mode t)   ; Makefile требует табы
  (setq-local tab-width 8)
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Используем встроенную подсветку, а не пользовательскую
  (setq-local font-lock-keywords-only t)

  ;; Простая вставка таба
  (local-set-key (kbd "TAB") (lambda () (interactive) (insert "\t")))

  ;; Полезные горячие клавиши
  (local-set-key (kbd "C-c C-c") (lambda () (interactive) (compile "make")))
  (local-set-key (kbd "C-c C-k") (lambda () (interactive) (compile "make clean")))
  (local-set-key (kbd "C-c C-s") (lambda () (interactive) (compile "make -n")))

  (message "Makefile mode ready (using built-in highlighting)"))

;; Хук для настройки
(add-hook 'makefile-gmake-mode-hook 'my/makefile-simple-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Инструменты (GDB, Doxymacs, LSP, Flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------- GDB -----------------
(use-package gdb-mi
  :ensure nil
  :init
  (add-to-list 'exec-path "/usr/bin/")
  :custom
  (gdb-many-windows nil)
  (gdb-async t)  ;; ВМЕСТО устаревшего target-async
  (gdb-show-main t)
  :bind
  ("<f5>" . gud-cont)
  ("<f6>" . gud-next)
  ("<f8>" . gud-step)
  ("<f9>" . gud-break)
  ;; :config
  ;; ;; Удаляем старый advice, он больше не нужен
  ;; (advice-remove 'gdb-input 'fix-gdb-deprecated-async)
  )

;; ----------------- Doxygen (без автосохранения) -----------------
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

;; ----------------- Flycheck (синтаксис) -----------------
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Загружаем локальные настройки если есть
  (when (file-exists-p ".emacs.flycheck.el")
    (load ".emacs.flycheck.el")))

;; ----------------- LSP (полная конфигурация) -----------------
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-log-io nil)  ; Отключаем логирование (ускоряет)
  :custom
  (lsp-idle-delay 0.500)  ; Задержка перед анализом
  (lsp-completion-provider :capf)  ; Более быстрое автодополнение
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable t)
  :hook
  ((c-mode c++-mode verilog-mode yaml-mode python-mode) . lsp-deferred)
  :config
  ;; Настройки для clangd (рекомендуется для C/C++)
  (setq lsp-clients-clangd-args '("--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--limit-references=1000"
                                  "--limit-results=1000"))

  ;; Игнорируем большие файлы
  (setq lsp-file-watch-threshold 2000)

  ;; Включить дополнительные возможности
  (lsp-enable-which-key-integration t))

;; Автодополнение через LSP
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :config
  (global-company-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8.1 Lisp и SKILL Development (Cadence)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка SLIME для Common Lisp
(use-package slime
  :custom
  (slime-lisp-implementations
   '((sbcl ("sbcl" "--dynamic-space-size" "2000"))
     (ccl ("ccl"))
     (clisp ("clisp"))
     (skill ("/opt/cadence/IC618/bin/skill" "-i"))))  ; Для Cadence SKILL
  (slime-default-lisp 'sbcl)
  (slime-net-coding-system 'utf-8-unix)
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :config
  (slime-setup '(slime-fancy slime-asdf slime-banner))

  ;; Автозагрузка Lisp файлов
  (add-hook 'lisp-mode-hook
            (lambda ()
              (unless (slime-connected-p)
                (save-excursion
                  (slime))))))

;; Auto-complete для SLIME
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'slime-fuzzy-complete-symbol))

;; Подсветка парных скобок
(use-package highlight-parentheses
  :hook (lisp-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors '("#2aa198" "#268bd2" "#d33682" "#cb4b16"))
  (highlight-parentheses-background-colors '("gray10")))

;; Разноцветные скобки
(use-package rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode))

;; Структурное редактирование Lisp
(use-package paredit
  :hook (lisp-mode . enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-(" . paredit-wrap-round)
              ("M-[" . paredit-wrap-square)
              ("M-{" . paredit-wrap-curly))
  :custom
  (paredit-space-for-delimiter-predicates nil))

;; Навигация по S-выражениям
(use-package lispy
  :hook (lisp-mode . lispy-mode)
  :custom
  (lispy-key-theme '(special lispy c-digits)))

;; Дополнительная подсветка для Lisp
(use-package lisp-extra-font-lock
  :hook (lisp-mode . lisp-extra-font-lock-mode))

;; АЛЬТЕРНАТИВНАЯ КОНФИГУРАЦИЯ SKILL-MODE (если основной способ не работает)
(use-package skill-mode
  :if (locate-file "skillMode.el" (list "~/.emacs.d/manual-packages/skill-mode/"))
  :load-path "~/.emacs.d/manual-packages/skill-mode/"
  :mode (("\\.il\\'" . skill-mode)
         ("\\.ils\\'" . skill-mode)
         ("\\.cdf\\'" . skill-mode)
         ("\\.bdf\\'" . skill-mode)
         ("\\.lef\\'" . skill-mode)
         ("\\.def\\'" . skill-mode))
  :config
  (setq skill-indent-level 2
        skill-electric-indent t
        skill-comment-column 40
        skill-prompt-for-skill-path nil))

;; Специальные настройки для SKILL (Cadence)
(defun my/skill-mode-setup ()
  "Настройки для режима SKILL (Cadence)."
  (interactive)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local lisp-indent-offset 2)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-column 40)
  (setq-local comment-multi-line t)

  ;; Специфичные для SKILL ключевые слова
  (font-lock-add-keywords
   'lisp-mode
   '(("\\<\\(let\\|foreach\\|prog\\|cond\\|when\\|unless\\|case\\)\\>"
      1 font-lock-keyword-face)
     ("\\<\\(defun\\|defmacro\\|defstruct\\|deftemplate\\)\\>"
      1 font-lock-type-face)
     ("\\<\\(t\\|nil\\|pi\\|inf\\|minusInf\\)\\>"
      1 font-lock-constant-face)
     ("\\<\\(lambda\\|closure\\)\\>"
      1 font-lock-function-name-face)
     ;; Cadence SKILL специфичные функции
     ("\\<\\(axl\\|db\\|ge\\|leCreate\\)\\>"
      1 font-lock-builtin-face)))

  ;; Пользовательские команды для SKILL
  (local-set-key (kbd "C-c C-e") 'slime-eval-last-expression)
  (local-set-key (kbd "C-c C-b") 'slime-eval-buffer)
  (local-set-key (kbd "C-c C-r") 'slime-eval-region)
  (local-set-key (kbd "C-c C-c") 'slime-compile-defun)
  (local-set-key (kbd "C-c C-k") 'slime-compile-and-load-file)

  (message "SKILL mode configured for Cadence"))

;; Автоматическая настройка для файлов SKILL
(add-hook 'lisp-mode-hook
          (lambda ()
            (when (or (string-match "\\.il\\'" (buffer-file-name))
                      (string-match "\\.ils\\'" (buffer-file-name))
                      (string-match "\\.cdf\\'" (buffer-file-name))
                      (string-match "\\.bdf\\'" (buffer-file-name))
                      (string-match "\\.lef\\'" (buffer-file-name))
                      (string-match "\\.def\\'" (buffer-file-name)))
              (my/skill-mode-setup))))

;; Интеграция с проектами Cadence
(defun my/find-cadence-project-root ()
  "Находит корень проекта Cadence по наличию файлов .cdsinit или .cdsenv."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (or (file-exists-p (expand-file-name ".cdsinit" dir))
                                (file-exists-p (expand-file-name ".cdsenv" dir))
                                (file-exists-p (expand-file-name "cds.lib" dir))))))

;; Функции для работы с Cadence
(defun my/cadence-run-skill (expr)
  "Выполнить выражение SKILL в Cadence."
  (interactive "sSKILL expression: ")
  (let ((skill-cmd (concat "skill -i \"" expr "\"")))
    (compile skill-cmd)))

(defun my/cadence-load-skill-file (file)
  "Загрузить файл SKILL в Cadence."
  (interactive "fLoad SKILL file: ")
  (let ((skill-cmd (format "skill -i \"load \\\"%s\\\"\"" file)))
    (compile skill-cmd)))

(defun my/cadence-connect ()
  "Подключиться к запущенному экземпляру Cadence."
  (interactive)
  (let ((default-directory (my/find-cadence-project-root)))
    (when default-directory
      (async-shell-command "virtuoso &" "*Cadence*")
      (message "Cadence Virtuoso запущен"))))

;; Настройки для Common Lisp проектов
(defun my/lisp-project-p ()
  "Определяет, является ли текущий проект Lisp проектом."
  (or (locate-dominating-file default-directory "*.asd")
      (locate-dominating-file default-directory "*.lisp")
      (locate-dominating-file default-directory "Makefile")))

;; Интеграция с системой сборки ASDF
(use-package slime-asdf
  :after slime
  :config
  ;; Правильный способ добавления в contribs
  (with-eval-after-load 'slime
    (require 'slime-asdf)
    (unless (memq 'slime-asdf slime-contribs)
      (setq slime-contribs (cons 'slime-asdf slime-contribs)))))

;; REPL настройки
(use-package slime-repl
  :after slime
  :custom
  (slime-repl-history-file "~/.emacs.d/slime-history")
  (slime-repl-history-size 1000)
  (slime-repl-return-behaviour :send-and-go)
  :hook (slime-repl-mode . (lambda ()
                             (company-mode 1)
                             (paredit-mode 1))))

;; ----------------- Project management -----------------
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-list-file "~/.emacs.d/projects.el")

  ;; Собственный project-switch-project для удобства
  (defun my/project-switch-project (dir)
    "Переключиться на проект в DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir))
      (project-switch-project dir)))

  (define-key project-prefix-map (kbd "s") 'my/project-switch-project))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8.2 MCU Debugging Framework (упрощенная версия)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Загружаем модуль отладки микроконтроллеров
(load (expand-file-name "mcu-debug.el" user-emacs-directory) t 'nomessage)

;; Основные функции MCU
(defun my/check-mcu-targets ()
  "Проверить загруженные цели MCU."
  (interactive)
  (if (boundp 'mcu-debug-targets)
      (if mcu-debug-targets
          (progn
            (message "Загружено %d целей:" (length mcu-debug-targets))
            (dolist (target mcu-debug-targets)
              (message "  - %s" (car target))))
        (message "Цели MCU не загружены. Проверьте директорию шаблонов: %s"
                 mcu-debug-templates-dir))
    (message "Модуль MCU Debugging не загружен")))

(defun my/check-mcu-dependencies ()
  "Проверить наличие всех необходимых программ для отладки MCU."
  (interactive)
  (message "=== Проверка зависимостей MCU ===")
  (let* ((tools '(("avr-gcc" . "Компилятор AVR")
                  ("avr-gdb" . "Отладчик AVR")
                  ("avrdude" . "Программатор AVR")
                  ("simavr" . "Симулятор AVR")
                  ("make" . "Система сборки")))
         (missing '()))
    (dolist (tool tools)
      (let ((name (car tool))
            (desc (cdr tool)))
        (if (executable-find name)
            (message "✓ %s (%s) найден" name desc)
          (progn
            (message "✗ %s (%s) НЕ найден" name desc)
            (push (format "%s (%s)" name desc) missing)))))
    (if missing
        (progn
          (message "\nОтсутствующие программы:")
          (dolist (m missing)
            (message "  - %s" m))
          (message "\nУстановите недостающие программы командой:")
          (message "sudo apt-get install gcc-avr avr-libc avrdude simavr"))
      (message "\nВсе зависимости удовлетворены!"))))

;; Добавляем команды в глобальную карту
(define-key my-global-map (kbd "T") 'my/check-mcu-targets)
(define-key my-global-map (kbd "D") 'my/check-mcu-dependencies)

;; Сообщение о готовности MCU модуля
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (featurep 'mcu-debug)
              (message "Модуль MCU Debugging готов. Используйте C-c g m для доступа к командам.")
              (message "  f - прошить (выбор режима debug/release)")
              (message "  s - запустить симуляцию")
              (message "  u - отладка через UART")
              (message "  b - переключить режим сборки"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Org Mode (Заметки и Задачи в иерархии проекта)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure nil
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c o" . org-open-at-point))
  :preface
  (declare-function org-save-all-org-buffers "org")
  (defun my/org-save-all-before-agenda (&rest _)
    "Сохранить все буферы Org перед показом agenda."
    (interactive)
    (when (featurep 'org)
      (org-save-all-org-buffers)))

  (defun my/org-current-project-notes ()
    "Ищет Makefile и возвращает путь к notes.org."
    (let ((project-dir (locate-dominating-file default-directory "Makefile")))
      (if project-dir
          (expand-file-name "notes.org" project-dir)
        (expand-file-name "notes.org" "~/org/"))))

  (defun my/force-auto-fill ()
    "Включить auto-fill в Org режиме."
    (auto-fill-mode 1))

  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org"))
  (org-capture-templates
   '(("t" "Задачка (Глобальная)" entry
      (file+headline "~/org/tasks.org" "Входящие")
      "* TODO %?\n  Создано: %U\n  %a")
     ("n" "Заметка к проекту (ex_*)" entry
      (file+headline my/org-current-project-notes "Notes")
      "* %?\n  Дата: %U\n  Контекст: %a\n  %i")
     ("j" "Journal" entry
      (file+datetree "~/org/journal.org")
      "* %?\n  %U")))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-hide-leading-stars t)
  (org-ellipsis " ▼")
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  :config
  ;; Настройки отображения
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t)

  ;; Включаем физический перенос
  (add-hook 'org-mode-hook 'my/force-auto-fill)

  ;; Автосохранение перед agenda
  (advice-add 'org-agenda :before #'my/org-save-all-before-agenda)

  ;; Экспорт
  (require 'ox-latex)
  (setq org-latex-pdf-process '("pdflatex -interaction nonstopmode %f"
                                "pdflatex -interaction nonstopmode %f")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Навигация и Дополнительно (GDB IDE - Custom Proportions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'gdb-mi)

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
                           (gdb-stack-buffer-mode . gdb-stack)
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
                                (and (window-live-p w)
                                     (fboundp 'purpose-window-purpose-get)
                                     (eq (purpose-window-purpose-get w) purp)))
                              (window-list))))
        (when (and buf win (window-live-p win))
          (set-window-buffer win buf)
          (set-window-dedicated-p win t))))))

(defun my/apply-gdb-layout ()
  "Построение IDE во втором фрейме с пропорциями 10-10-10-10-60 справа."
  (interactive)
  (delete-other-windows)
  (let* ((left-win (selected-window))
         (right-win
          (split-window left-win (floor (* (window-total-width) 0.55)) 'right)))

    ;; Проверяем, что разделение прошло успешно
    (unless (window-live-p right-win)
      (error "Не удалось создать правое окно"))

    ;; ЛЕВАЯ КОЛОННА
    (with-selected-window left-win
      (let* ((w-gdb (split-window-below (floor (* (window-height) 0.7))))
             (w-io  (with-selected-window w-gdb
                      (split-window-below (floor (* (window-height) 0.5))))))
        (when (window-live-p w-gdb)
          (purpose-set-window-purpose 'edit left-win)
          (purpose-set-window-purpose 'gdb-out w-gdb))
        (when (window-live-p w-io)
          (purpose-set-window-purpose 'gdb-io w-io))
        (switch-to-buffer (other-buffer (current-buffer) t))))

    ;; ПРАВАЯ КОЛОННА (Настройка пропорций)
    (with-selected-window right-win
      (let* ((total-h (window-height))
             (h-step (floor (* total-h 0.1)))
             w-stack w-locals w-regs w-breaks)

        (setq w-stack (split-window-below h-step))
        (when (window-live-p w-stack)
          (purpose-set-window-purpose 'gdb-disasm right-win)
          (purpose-set-window-purpose 'gdb-stack w-stack)

          (with-selected-window w-stack
            (setq w-locals (split-window-below h-step)))

          (when (window-live-p w-locals)
            (purpose-set-window-purpose 'gdb-locals w-locals)

            (with-selected-window w-locals
              (setq w-regs (split-window-below h-step)))

            (when (window-live-p w-regs)
              (purpose-set-window-purpose 'gdb-regs w-regs)

              (with-selected-window w-regs
                (setq w-breaks (split-window-below h-step)))

              (when (window-live-p w-breaks)
                (purpose-set-window-purpose 'gdb-breaks w-breaks)))))))

    ;; Генерация буферов (только если окна существуют)
    (gdb-display-io-buffer)
    (gdb-display-gdb-buffer)
    (gdb-display-registers-buffer)
    (gdb-display-locals-buffer)
    (gdb-display-stack-buffer)
    (gdb-display-breakpoints-buffer)
    (gdb-display-disassembly-buffer)

    (run-with-timer 0.5 nil #'my/populate-gdb-windows)))

(use-package window-purpose
  :config
  (purpose-mode 1)
  ;; Разрешаем Helm открываться поверх Purpose-окон
  (add-to-list 'purpose-user-regexp-purposes '("^\\*helm" . nil))

  ;; Правильное переопределение find-file для Purpose
  (defun purpose-find-file-overload (&optional arg)
    "Переопределение find-file для работы с Purpose."
    (interactive "P")
    (let ((purpose-mode nil))  ;; Временно отключаем Purpose для этой команды
      (call-interactively 'helm-find-files)))

  ;; Используем advice вместо fset для более чистого переопределения
  (advice-add 'find-file :override #'purpose-find-file-overload)

  ;; Обновим load-purpose-mode
  (defun load-purpose-mode ()
    (interactive)
    ;; Прижимаем основной фрейм влево
    (set-frame-parameter nil 'left 0)
    (set-frame-parameter nil 'top 0)
    (my/create-4-pane-grid)

    (let ((f2 (make-frame '((name . "GDB-DEBUG")
                            (width . 200)
                            (height . 85)
                            (left . 1200)
                            (top . 0)))))
      (with-selected-frame f2
        ;; Гарантируем, что helm и which-key работают
        (my/ensure-helm-in-frame f2)
        (my/ensure-which-key-in-frame f2)
        (call-interactively 'gdb)
        (my/apply-gdb-layout))))
  :bind (("M-L" . load-purpose-mode)
         ("M-g L" . my/apply-gdb-layout)))

(global-set-key (kbd "C-x /") (lambda () (interactive)
                                (find-file "~/.emacs.d/init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Git (Magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-auto-revert-mode t)
  :config
  ;; Включить автоматическое обновление статуса
  (magit-auto-revert-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12. Docker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package docker
  :bind ("C-c M-d" . docker)
  :custom
  (docker-command "docker")
  (docker-run-as-root t))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 13. Дополнительные инструменты
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Сниппеты (шаблоны кода)
(use-package yasnippet
  :config
  (yas-global-mode)
  :bind (:map yas-minor-mode-map
              ("C-c y n" . yas-new-snippet)
              ("C-c y e" . yas-expand)
              ("C-c y i" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :after yasnippet)

;; Поиск по проекту
(use-package rg
  :if (executable-find "rg")
  :bind ("C-c s" . rg-menu))

;; Просмотр PDF внутри Emacs
(use-package pdf-tools
  :if (file-exists-p "/usr/include/poppler.h")  ; Проверка зависимостей
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

;; Terminal внутри Emacs
(use-package vterm
  :if (executable-find "libvterm")
  :bind ("C-c t" . vterm)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/bash"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14. Команды для управления пакетами
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Добавляем команды для удобства
(defun my/show-packages-status ()
  "Показывает статус всех пакетов из my/core-packages."
  (interactive)
  (message "=== Статус пакетов ===")
  (dolist (pkg-spec my/core-packages)
    (let* ((pkg-name (car pkg-spec))
           (pkg-str (symbol-name pkg-name))
           (pkg-dir (expand-file-name pkg-str my-manual-packages-dir))
           (installed-p (or (package-installed-p pkg-name)
                            (featurep pkg-name)
                            (file-exists-p pkg-dir))))
      (message "%s: %s" pkg-str
               (cond
                ((package-installed-p pkg-name) "✓ Установлен (package.el)")
                ((featurep pkg-name) "✓ Загружен (manual-packages)")
                ((file-exists-p pkg-dir) "✓ Файлы есть (manual-packages)")
                (t "✗ Отсутствует"))))))

;; Проверка и исправление структуры проблемных пакетов
(defun my/fix-problematic-packages ()
  "Проверяет и исправляет структуру проблемных пакетов."
  (interactive)
  (dolist (pkg-spec my/core-packages)
    (let ((pkg-name (car pkg-spec))
          (pkg-dir (expand-file-name (symbol-name (car pkg-spec))
                                     my-manual-packages-dir)))
      (when (file-exists-p pkg-dir)
        (my/fix-package-structure pkg-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14.1 Унифицированные префиксные команды (ИСПРАВЛЕННЫЕ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Основной префикс для пользовательских команд
;; Привязываем глобальную карту команд к C-c g
(global-set-key (kbd "C-c g") my-global-map) ;; g = Global

;; Управление пакетами
(define-prefix-command 'my-packages-map)
(define-key my-global-map (kbd "p") my-packages-map)
(define-key my-packages-map (kbd "s") 'my/show-packages-status)
(define-key my-packages-map (kbd "d") 'my/download-missing-packages)
(define-key my-packages-map (kbd "u") 'my/update-all-packages)
(define-key my-packages-map (kbd "c") 'my/clean-manual-packages)
(define-key my-packages-map (kbd "f") 'my/fix-problematic-packages)

;; Lisp/SKILL разработка
(define-prefix-command 'my-lisp-map)
(define-key my-global-map (kbd "l") my-lisp-map)
(define-key my-lisp-map (kbd "s") 'slime)
(define-key my-lisp-map (kbd "e") 'slime-eval-last-expression)
(define-key my-lisp-map (kbd "b") 'slime-eval-buffer)
(define-key my-lisp-map (kbd "r") 'slime-repl)
(define-key my-lisp-map (kbd "c") 'my/cadence-connect)
(define-key my-lisp-map (kbd "v") 'my/cadence-run-skill)
(define-key my-lisp-map (kbd "f") 'my/cadence-load-skill-file)

;; ;; EAF управление
;; (define-prefix-command 'my-eaf-map)
;; (define-key my-global-map (kbd "e") my-eaf-map)
;; (define-key my-eaf-map (kbd "b") 'eaf-open-browser-wrapper)
;; (define-key my-eaf-map (kbd "g") 'eaf-open-google)
;; (define-key my-eaf-map (kbd "f") 'my/eaf-open-file-manager)
;; (define-key my-eaf-map (kbd "t") 'my/eaf-open-terminal)
;; (define-key my-eaf-map (kbd "l") 'eaf-login-google)
;; (define-key my-eaf-map (kbd "u") 'eaf-switch-user-agent)
;; (define-key my-eaf-map (kbd "c") 'eaf-clear-cache)

;; Поиск
(define-prefix-command 'my-search-map)
(define-key my-global-map (kbd "s") my-search-map)
(define-key my-search-map (kbd "s") 'helm-occur)
(define-key my-search-map (kbd "f") 'helm-find-files)
(define-key my-search-map (kbd "p") 'helm-projectile)
(define-key my-search-map (kbd "r") 'rg-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15. EAF (Emacs Application Framework) - ТОЛЬКО ТЕКСТОВЫЙ БРАУЗЕР
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Функция для проверки и загрузки EAF
;; (defun my/ensure-eaf-loaded ()
;;   "Проверяет и загружает EAF с настройками."
;;   (let ((eaf-dir "~/.emacs.d/site-lisp/emacs-application-framework"))
;;     (if (not (file-directory-p eaf-dir))
;;         (progn
;;           (message "EAF не найден в %s. Установите его вручную." eaf-dir)
;;           nil)
;;       (add-to-list 'load-path eaf-dir)
;;       (condition-case err
;;           (progn
;;             (require 'eaf)
;;             (message "EAF базовая загрузка успешна")
;;             t)
;;         (error
;;          (message "Ошибка загрузки EAF: %s" err)
;;          nil)))))

;; ;; Основные настройки EAF (вызываются сразу после загрузки)
;; (defun my/setup-eaf-basic ()
;;   "Настраивает основные параметры EAF."
;;   (setq eaf-python-command "python3"
;;         eaf-start-python-process-when-require t
;;         eaf-config-location "~/.emacs.d/eaf-config.el")

;;   ;; Убедимся что директория для конфигурации существует
;;   (unless (file-exists-p eaf-config-location)
;;     (make-directory (file-name-directory eaf-config-location) t)))

;; ;; Команды для быстрого доступа к EAF браузеру
;; (defun eaf-open-browser-wrapper (url)
;;   "Обертка для eaf-open-browser с проверкой."
;;   (interactive "sURL: ")
;;   (if (fboundp 'eaf-open-browser)
;;       (eaf-open-browser url)
;;     (message "Функция eaf-open-browser не определена. Перезагрузите Emacs.")))

;; (defun eaf-open-google ()
;;   "Открыть Google."
;;   (interactive)
;;   (eaf-open-browser-wrapper "https://www.google.com"))

;; (defun my/eaf-open-terminal ()
;;   "Открыть терминал EAF."
;;   (interactive)
;;   (if (fboundp 'eaf-open)
;;       (eaf-open "~/" "terminal")
;;     (message "EAF не загружен")))

;; ;; Функции для решения проблем с браузером
;; (defun eaf-clear-cache ()
;;   "Очистить кэш EAF."
;;   (interactive)
;;   (shell-command "rm -rf ~/.emacs.d/eaf/* 2>/dev/null")
;;   (message "Кэш EAF очищен"))

;; (defun eaf-switch-user-agent (agent)
;;   "Переключить user-agent браузера."
;;   (interactive
;;    (list (completing-read "Выберите user-agent: "
;;                           '("Chrome Windows"
;;                             "Chrome Linux"
;;                             "Firefox Windows"
;;                             "Firefox Linux"
;;                             "Safari Mac"
;;                             "Edge Windows"))))
;;   (when (boundp 'eaf-browser-user-agent)
;;     (setq eaf-browser-user-agent
;;           (cond
;;            ((equal agent "Chrome Windows")
;;             "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
;;            ((equal agent "Chrome Linux")
;;             "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
;;            ((equal agent "Firefox Windows")
;;             "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/120.0")
;;            ((equal agent "Firefox Linux")
;;             "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/120.0")
;;            ((equal agent "Safari Mac")
;;             "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15")
;;            ((equal agent "Edge Windows")
;;             "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0")
;;            (t eaf-browser-user-agent)))
;;     (message "User-agent изменен на: %s" agent)))

;; (defun eaf-login-google ()
;;   "Открыть Google с настройки для входа в аккаунт."
;;   (interactive)
;;   (let ((original-user-agent (when (boundp 'eaf-browser-user-agent) eaf-browser-user-agent))
;;         (original-cookie (when (boundp 'eaf-browser-enable-cookie) eaf-browser-enable-cookie)))

;;     ;; Устанавливаем настройки для Google
;;     (when (boundp 'eaf-browser-user-agent)
;;       (setq eaf-browser-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"))

;;     (when (boundp 'eaf-browser-enable-cookie)
;;       (setq eaf-browser-enable-cookie t))

;;     ;; Открываем Google
;;     (eaf-open-browser-wrapper "https://accounts.google.com")

;;     ;; Восстанавливаем настройки через 10 секунд
;;     (run-with-timer 10 nil
;;                     (lambda ()
;;                       (when (and (boundp 'eaf-browser-user-agent) original-user-agent)
;;                         (setq eaf-browser-user-agent original-user-agent))
;;                       (when (and (boundp 'eaf-browser-enable-cookie) original-cookie)
;;                         (setq eaf-browser-enable-cookie original-cookie))
;;                       (message "Настройки EAF восстановлены")))))

;; (defun eaf-open-google-incognito ()
;;   "Открыть Google в 'инкогнито' режиме (с очищенным кэшем)."
;;   (interactive)
;;   (eaf-clear-cache)
;;   (eaf-open-browser-wrapper "https://www.google.com"))

;; ;; Пробуем загрузить EAF при старте
;; (when (my/ensure-eaf-loaded)
;;   (my/setup-eaf-basic)

;;   ;; Привязываем команды EAF к префиксной карте
;;   (define-key my-eaf-map (kbd "b") 'eaf-open-browser-wrapper)
;;   (define-key my-eaf-map (kbd "g") 'eaf-open-google)
;;   (define-key my-eaf-map (kbd "t") 'my/eaf-open-terminal)
;;   (define-key my-eaf-map (kbd "l") 'eaf-login-google)
;;   (define-key my-eaf-map (kbd "i") 'eaf-open-google-incognito)
;;   (define-key my-eaf-map (kbd "u") 'eaf-switch-user-agent)
;;   (define-key my-eaf-map (kbd "c") 'eaf-clear-cache)

;;   ;; Откладываем загрузку модулей и настройку клавиш
;;   (run-with-timer 3 nil
;;                   (lambda ()
;;                     ;; Автоматическая загрузка модулей EAF (только текстовые)
;;                     (dolist (module '(eaf-browser
;;                                       eaf-pdf-viewer
;;                                       eaf-image-viewer
;;                                       eaf-markdown-previewer
;;                                       eaf-terminal
;;                                       eaf-org-previewer
;;                                       eaf-rss-reader))
;;                       (condition-case err
;;                           (progn
;;                             (require module)
;;                             (message "EAF модуль %s загружен" module))
;;                         (error
;;                          (message "Не удалось загрузить модуль %s: %s" module err))))

;;                     ;; Настройка параметров после загрузки модулей
;;                     (when (boundp 'eaf-browser-user-agent)
;;                       (setq eaf-browser-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
;;                             eaf-browser-enable-javascript t
;;                             eaf-browser-enable-cookie t
;;                             eaf-browser-remember-history t
;;                             eaf-browser-enable-password t
;;                             eaf-browser-continue-where-left-off t
;;                             eaf-browser-enable-adblocker t
;;                             eaf-browser-default-zoom 1.0
;;                             eaf-browser-download-path "~/Downloads"))

;;                     (message "Все модули EAF загружены и настроены!"))))

;; ;; Настройка горячих клавиш для браузера EAF
;; (with-eval-after-load 'eaf-browser
;;   ;; Добавляем хук для настройки клавиш браузера
;;   (defun my/setup-eaf-browser-keys-hook ()
;;     "Настраивает горячие клавиши для браузера EAF."
;;     (when (derived-mode-p 'eaf-browser-mode)
;;       ;; Перенос строки по Shift+RET
;;       (local-set-key (kbd "<S-return>") (lambda () (interactive)
;;                                           (eaf-call-async "execute_function" eaf--buffer-id "insert_newline")))

;;       ;; Основные клавиши навигации
;;       (local-set-key (kbd "M-<left>") 'eaf-browser-history-back)
;;       (local-set-key (kbd "M-<right>") 'eaf-browser-history-forward)
;;       (local-set-key (kbd "C-<left>") 'eaf-browser-history-back)
;;       (local-set-key (kbd "C-<right>") 'eaf-browser-history-forward)
;;       (local-set-key (kbd "C-c b") 'eaf-browser-history-back)
;;       (local-set-key (kbd "C-c f") 'eaf-browser-history-forward)

;;       ;; Масштабирование
;;       (local-set-key (kbd "C--") 'eaf-browser-zoom-out)
;;       (local-set-key (kbd "C-=") 'eaf-browser-zoom-in)
;;       (local-set-key (kbd "C-0") 'eaf-browser-zoom-reset)

;;       ;; Управление вкладками
;;       (local-set-key (kbd "C-x t") 'eaf-create-blank-tab)
;;       (local-set-key (kbd "C-x k") 'eaf-close-tab)
;;       (local-set-key (kbd "C-x <right>") 'eaf-next-tab)
;;       (local-set-key (kbd "C-x <left>") 'eaf-prev-tab)

;;       ;; Прокрутка
;;       (local-set-key (kbd "C-v") 'eaf-browser-scroll-up)
;;       (local-set-key (kbd "M-v") 'eaf-browser-scroll-down)
;;       (local-set-key (kbd "C-x >") 'eaf-browser-scroll-right)
;;       (local-set-key (kbd "C-x <") 'eaf-browser-scroll-left)

;;       ;; Поиск
;;       (local-set-key (kbd "C-s") 'eaf-browser-search-text)
;;       (local-set-key (kbd "C-r") 'eaf-browser-clear-search)

;;       ;; Обновление
;;       (local-set-key (kbd "F5") 'eaf-browser-refresh)
;;       (local-set-key (kbd "C-x r") 'eaf-browser-refresh)
;;       (local-set-key (kbd "C-x d") 'eaf-browser-toggle-dark-mode)))

;;   (add-hook 'eaf-browser-mode-hook 'my/setup-eaf-browser-keys-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16. Завершение инициализации
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Загружаем кастомные настройки если есть
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;; Сообщение о готовности
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs готов! Загрузка заняла %.2f секунд"
                     (float-time (time-subtract after-init-time before-init-time)))
            ;; Исправляем структуру проблемных пакетов
            (run-with-timer 0.5 nil 'my/fix-problematic-packages)
            ;; Показываем статус пакетов при запуске
            (run-with-timer 1.0 nil 'my/show-packages-status)
            ;; Показываем доступные команды
            (message "Команды управления пакетами: C-c g p [s]татус, [d]ownload, [u]pdate, [c]lean, [f]ix")
            (message "Команды для Lisp/SKILL: C-c g l [s]lime, [e]val, [b]uffer, [r]epl, [c]adence")
            ;; (message "Команды для EAF: C-c g e [b]rowser, [g]oogle, [m]ail, [f]ile-manager")
            (message "Команды поиска: C-c g s [s]earch, [f]iles, [p]roject, [r]g")
            ;; Дополнительная информация
            (when (featurep 'eaf)
              (message "EAF загружен. Будет использоваться только как текстовый браузер."))))

;; Восстанавливаем GC на нормальный уровень
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))  ; 16MB

(provide 'init)
