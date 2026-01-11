;;; mcu-debug.el --- MCU Debugging Framework for Embedded Projects -*- lexical-binding: t -*-

;; Copyright (C) 2024 harkut
;; Author: harkut <yanovets.vasya@gmail.com>
;; Version: 1.7
;; Keywords: embedded, mcu, avr, arm, gdb, debug, release

;;; Commentary:
;; Модуль для отладки микроконтроллеров через GDB с поддержкой
;; AVR, ARM и других архитектур. Использует существующую структуру
;; проектов и Makefile-ы.

;;; Code:

(require 'gdb-mi)
(require 'compile)

(defgroup mcu-debug nil
  "Настройки отладки микроконтроллеров."
  :group 'tools
  :group 'embedded)

(defcustom mcu-debug-templates-dir "~/.emacs.d/mcu-debug-templates/"
  "Директория для шаблонов конфигурации микроконтроллеров."
  :type 'directory
  :group 'mcu-debug)

(defcustom mcu-debug-default-uart-port 3333
  "Порт по умолчанию для UART отладки."
  :type 'integer
  :group 'mcu-debug)

(defcustom mcu-debug-default-build-mode "debug"
  "Режим сборки по умолчанию: debug или release."
  :type '(choice (const "debug") (const "release"))
  :group 'mcu-debug)

(defcustom mcu-debug-project-root-markers
  '("Makefile" ".git" "Doxyfile")
  "Маркеры корня проекта для автоопределения."
  :type 'list
  :group 'mcu-debug)

(defvar mcu-debug-targets '()
  "Список настроенных целевых микроконтроллеров.
Каждый элемент - cons-ячейка (NAME . PLIST), где PLIST содержит настройки.")

;; ============================================================================
;; Встроенные конфигурации для популярных микроконтроллеров
;; ============================================================================

(defconst mcu-debug-builtin-targets
  '(("AVR-ATMega328"
     (:gdb-executable "avr-gdb"
      :gdb-args ""
      :simulator "simavr"
      :simulator-args "-m atmega328 -f 16000000 -g"
      :uart-debugger "avrdude"
      :uart-args "-c arduino -p atmega328p -P /dev/ttyACM0 -b 115200"
      :elf-suffix ".elf"
      :hex-suffix ".hex"
      :flash-make-target-debug "flash"
      :flash-make-target-release "flash-release"
      :debug-cflags "-g -Og"
      :release-cflags "-Os -flto"
      :make-variables ((MCU . "atmega328p")
                       (F_CPU . 16000000)
                       (CC . "avr-gcc")
                       (OBJCOPY . "avr-objcopy")
                       (OBJDUMP . "avr-objdump")
                       (SIZE . "avr-size")
                       (AVRDUDE . "avrdude")
                       (PROGRAMMER_TYPE . "arduino")
                       (PROGRAMMER_BAUD . 57600))
      :memory-map ((flash :start 0x0 :size 0x8000)
                   (sram  :start 0x100 :size 0x800))))

    ("AVR-ATMega2560"
     (:gdb-executable "avr-gdb"
      :gdb-args ""
      :simulator "simavr"
      :simulator-args "-m atmega2560 -f 16000000 -g"
      :uart-debugger "avrdude"
      :uart-args "-c wiring -p atmega2560 -P /dev/ttyACM0 -b 115200"
      :elf-suffix ".elf"
      :hex-suffix ".hex"
      :flash-make-target-debug "flash"
      :flash-make-target-release "flash-release"
      :debug-cflags "-g -Og"
      :release-cflags "-Os -flto"
      :make-variables ((MCU . "atmega2560")
                       (F_CPU . 16000000)
                       (CC . "avr-gcc")
                       (OBJCOPY . "avr-objcopy")
                       (OBJDUMP . "avr-objdump")
                       (SIZE . "avr-size")
                       (AVRDUDE . "avrdude")
                       (PROGRAMMER_TYPE . "wiring")
                       (PROGRAMMER_BAUD . 115200))
      :memory-map ((flash :start 0x0 :size 0x40000)
                   (sram  :start 0x200 :size 0x2000))))

    ("ARM-STM32F4"
     (:gdb-executable "arm-none-eabi-gdb"
      :gdb-args ""
      :simulator "qemu-system-arm"
      :simulator-args "-machine stm32f4-discovery -kernel"
      :uart-debugger "openocd"
      :uart-args "-f interface/stlink.cfg -f target/stm32f4x.cfg"
      :elf-suffix ".elf"
      :hex-suffix ".hex"
      :flash-make-target "flash"
      :debug-cflags "-g -Og"
      :release-cflags "-Os -flto"
      :make-variables ((MCU . "stm32f4")
                       (CC . "arm-none-eabi-gcc")
                       (OBJCOPY . "arm-none-eabi-objcopy")
                       (OBJDUMP . "arm-none-eabi-objdump")
                       (SIZE . "arm-none-eabi-size"))
      :memory-map ((flash :start 0x08000000 :size 0x100000)
                   (ram   :start 0x20000000 :size 0x30000)))))
  "Встроенные конфигурации для популярных микроконтроллеров.")

;; Создаем директорию для шаблонов
(unless (file-exists-p mcu-debug-templates-dir)
  (make-directory mcu-debug-templates-dir t))

;; ============================================================================
;; Вспомогательные функции
;; ============================================================================

(defun mcu-debug-find-project-root ()
  "Найти корень проекта на основе маркеров."
  (or (locate-dominating-file default-directory
                              (lambda (dir)
                                (cl-some (lambda (marker)
                                           (file-exists-p (expand-file-name marker dir)))
                                         mcu-debug-project-root-markers)))
      default-directory))

(defun mcu-debug-get-target-plist (target-name)
  "Получить plist конфигурации целевого микроконтроллера."
  (let ((target (assoc target-name mcu-debug-targets)))
    (unless target
      (error "Целевой микроконтроллер '%s' не найден" target-name))
    (cdr target)))

(defun mcu-debug-choose-build-mode ()
  "Выбрать режим сборки (debug или release)."
  (completing-read "Режим сборки: " '("debug" "release") nil t
                   mcu-debug-default-build-mode))

(defun mcu-debug-get-makefile-targets (project-dir)
  "Получить список целей из Makefile проекта."
  (let ((makefile (expand-file-name "Makefile" project-dir))
        targets)
    (when (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-zA-Z][a-zA-Z0-9_-]*\\):" nil t)
          (push (match-string 1) targets)))
      (reverse targets))))

(defun mcu-debug-build-make-command (project-dir target-name build-mode &optional make-target)
  "Построить команду make с переменными из конфигурации микроконтроллера."
  (let* ((plist (mcu-debug-get-target-plist target-name))
         (make-vars (plist-get plist :make-variables))
         (cmd (format "make -C %s BUILD_MODE=%s"
                      (shell-quote-argument project-dir)
                      build-mode)))

    ;; Добавляем переменные из конфигурации
    (when make-vars
      (dolist (var make-vars)
        (setq cmd (format "%s %s='%s'" cmd (car var) (cdr var)))))

    ;; Добавляем цель, если указана
    (when make-target
      (setq cmd (format "%s %s" cmd make-target)))

    cmd))

;; ============================================================================
;; Основные функции пользователя
;; ============================================================================

;;;###autoload
(defun mcu-debug-run-make-target ()
  "Выполнить цель Makefile для проекта с настройками из выбранного микроконтроллера."
  (interactive)

  (let* ((project-dir (mcu-debug-find-project-root))
         (build-mode (mcu-debug-choose-build-mode))
         (target-name (completing-read "Целевой MCU: "
                                       (mapcar 'car mcu-debug-targets)
                                       nil t "AVR-ATMega328"))
         (targets (mcu-debug-get-makefile-targets project-dir))
         (make-target (completing-read "Цель Makefile: " targets nil t))
         (cmd (mcu-debug-build-make-command project-dir target-name build-mode make-target)))

    (message "Выполняем команду: %s" cmd)
    (compile cmd)))

;;;###autoload
(defun mcu-debug-build-project (target-name build-mode)
  "Собрать проект в указанном режиме."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")
         (mcu-debug-choose-build-mode)))

  (let* ((project-dir (mcu-debug-find-project-root))
         (build-target (cond
                        ((string= build-mode "debug") "debug")
                        ((string= build-mode "release") "release")
                        (t "all")))
         (cmd (mcu-debug-build-make-command project-dir target-name build-mode build-target))
         (compilation-buffer-name "*MCU Build*"))

    (message "Сборка проекта в режиме %s..." build-mode)
    (compile cmd)))

;;;###autoload
(defun mcu-debug-flash-firmware (target-name)
  "Прошить firmware в целевой микроконтроллер."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (build-mode (mcu-debug-choose-build-mode))
         (plist (mcu-debug-get-target-plist target-name))
         (flash-target (cond
                        ((string= build-mode "debug")
                         (or (plist-get plist :flash-make-target-debug)
                             (plist-get plist :flash-make-target)
                             "flash"))
                        ((string= build-mode "release")
                         (or (plist-get plist :flash-make-target-release)
                             (concat (or (plist-get plist :flash-make-target) "flash") "-release")
                             "flash-release"))
                        (t (or (plist-get plist :flash-make-target) "flash")))))

    (when (y-or-n-p (format "Собрать проект перед прошивкой в режиме %s?" build-mode))
      (mcu-debug-build-project target-name build-mode))

    (let ((cmd (mcu-debug-build-make-command project-dir target-name build-mode flash-target)))
      (compile cmd))))

;;;###autoload
(defun mcu-debug-switch-build-mode ()
  "Переключить режим сборки между debug и release."
  (interactive)
  (setq mcu-debug-default-build-mode
        (if (string= mcu-debug-default-build-mode "debug")
            "release"
          "debug"))
  (message "Режим сборки изменен на: %s" mcu-debug-default-build-mode))

;; ============================================================================
;; Инициализация модуля
;; ============================================================================

;;;###autoload
(defun mcu-debug-initialize ()
  "Инициализировать модуль отладки микроконтроллеров."
  ;; Регистрируем встроенные цели
  (dolist (target mcu-debug-builtin-targets)
    (let ((name (car target))
          (config (cadr target)))
      (mcu-debug-register-target name config)))

  ;; Загружаем пользовательские шаблоны
  (when (file-exists-p mcu-debug-templates-dir)
    (dolist (file (directory-files mcu-debug-templates-dir t "\\.el$"))
      (condition-case err
          (progn
            (load file nil 'nomessage)
            (message "Загружен шаблон: %s" (file-name-nondirectory file)))
        (error
         (message "Ошибка загрузки шаблона %s: %s"
                  (file-name-nondirectory file) err)))))

  ;; Создаем карту команд MCU
  (defvar mcu-debug-global-map (make-sparse-keymap)
    "Глобальная карта команд для отладки MCU.")

  (define-key mcu-debug-global-map (kbd "s") 'mcu-debug-start-simulation)
  (define-key mcu-debug-global-map (kbd "u") 'mcu-debug-start-uart-debug)
  (define-key mcu-debug-global-map (kbd "f") 'mcu-debug-flash-firmware)
  (define-key mcu-debug-global-map (kbd "c") 'mcu-debug-run-make-target)
  (define-key mcu-debug-global-map (kbd "C") 'compile)
  (define-key mcu-debug-global-map (kbd "m") 'mcu-debug-run-make-target)
  (define-key mcu-debug-global-map (kbd "g") 'mcu-debug-generate-template)
  (define-key mcu-debug-global-map (kbd "l") 'mcu-debug-load-all-templates)
  (define-key mcu-debug-global-map (kbd "k") 'mcu-debug-stop-all-processes)
  (define-key mcu-debug-global-map (kbd "b") 'mcu-debug-switch-build-mode)

  ;; Привязываем MCU карту к my-global-map
  (when (boundp 'my-global-map)
    (define-key my-global-map (kbd "m") mcu-debug-global-map))

  (global-set-key (kbd "C-c g m") mcu-debug-global-map)

  (message "Модуль MCU Debugging инициализирован (%d целей)" (length mcu-debug-targets)))

;; Автоматическая инициализация
(mcu-debug-initialize)

(provide 'mcu-debug)

;;; mcu-debug.el ends here
