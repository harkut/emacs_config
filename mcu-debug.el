;;; mcu-debug.el --- MCU Debugging Framework for Embedded Projects -*- lexical-binding: t -*-

;; Copyright (C) 2024 harkut

;; Author: harkut <yanovets.vasya@gmail.com>
;; Version: 1.0
;; Keywords: embedded, mcu, avr, arm, gdb

;; Этот файл НЕ является частью GNU Emacs.

;;; Commentary:
;; Модуль для отладки микроконтроллеров через GDB с поддержкой
;; AVR, ARM и других архитектур. Использует существующую структуру
;; проектов и Makefile-ы.

;;; Code:

(require 'gdb-mi)
(require 'compile)
(require 'project)

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
      :flash-make-target "flash"
      :make-targets ("all" "clean" "flash" "fuses")
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
      :flash-make-target "flash"
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
      :memory-map ((flash :start 0x08000000 :size 0x100000)
                   (ram   :start 0x20000000 :size 0x30000)))))
  "Встроенные конфигурации для популярных микроконтроллеров.")

;; Создаем директорию для шаблонов
(unless (file-exists-p mcu-debug-templates-dir)
  (make-directory mcu-debug-templates-dir t))

;; ============================================================================
;; Утилиты для работы с проектами
;; ============================================================================

(defun mcu-debug-find-project-root ()
  "Найти корень проекта на основе маркеров."
  (or (locate-dominating-file default-directory
                              (lambda (dir)
                                (cl-some (lambda (marker)
                                           (file-exists-p (expand-file-name marker dir)))
                                         mcu-debug-project-root-markers)))
      default-directory))

(defun mcu-debug-get-project-name ()
  "Получить имя проекта из текущей директории."
  (file-name-nondirectory (directory-file-name (mcu-debug-find-project-root))))

(defun mcu-debug-find-elf-file (project-dir target-name)
  "Найти ELF файл в директории проекта."
  (let* ((target (assoc target-name mcu-debug-targets))
         (plist (cdr target))
         (elf-suffix (or (plist-get plist :elf-suffix) ".elf"))
         (bin-dir (expand-file-name "bin" project-dir))
         (elf-files (when (file-exists-p bin-dir)
                      (directory-files bin-dir t (concat "\\" elf-suffix "$")))))
    (cond
     ((= (length elf-files) 1) (car elf-files))
     ((> (length elf-files) 1)
      (completing-read "Выберите ELF файл: " elf-files nil t))
     (t (error "ELF файл не найден в %s. Соберите проект командой 'make all'" bin-dir)))))

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

;; ============================================================================
;; Регистрация и управление целями
;; ============================================================================

(defun mcu-debug-register-target (name config-plist)
  "Зарегистрировать новый целевой микроконтроллер."
  (let ((existing (assoc name mcu-debug-targets)))
    (if existing
        (setcdr existing config-plist)
      (push (cons name config-plist) mcu-debug-targets)))
  (message "Цель '%s' зарегистрирована" name))

(defun mcu-debug-get-target-plist (name)
  "Получить plist конфигурации целевого микроконтроллера."
  (let ((target (assoc name mcu-debug-targets)))
    (unless target
      (error "Целевой микроконтроллер '%s' не найден" name))
    (cdr target)))

(defun mcu-debug-ensure-gdb-executable (target-name)
  "Проверить доступность GDB для целевого микроконтроллера."
  (let* ((plist (mcu-debug-get-target-plist target-name))
         (gdb-exe (plist-get plist :gdb-executable)))
    (unless (executable-find gdb-exe)
      (error "GDB не найден: %s. Установите пакет с компилятором для этой архитектуры." gdb-exe))
    gdb-exe))

;; ============================================================================
;; Запуск симуляторов и отладчиков (ИСПРАВЛЕНО - ПРОСТОЙ ЗАПУСК)
;; ============================================================================

(defun mcu-debug-launch-simulator-simple (elf-file)
  "Простой запуск simavr в терминале."
  (let* ((elf-dir (file-name-directory elf-file))
         (elf-name (file-name-nondirectory elf-file))
         (cmd (format "cd '%s' && simavr -m atmega328 -f 16000000 -g '%s' &"
                      elf-dir elf-name)))

    (message "Запуск simavr командой: %s" cmd)

    ;; Запускаем в shell без захвата вывода
    (shell-command cmd)

    ;; Даем время на запуск
    (sleep-for 3)

    ;; Проверяем, слушает ли порт 1234
    (let ((port-open-p
           (zerop (call-process "sh" nil nil nil "-c"
                                "timeout 1 bash -c 'echo > /dev/tcp/localhost/1234' 2>/dev/null"))))
      (if port-open-p
          (progn
            (message "Simavr запущен и слушает порт 1234")
            t)
        (progn
          (message "ВНИМАНИЕ: Simavr не слушает порт 1234")
          nil)))))

(defun mcu-debug-launch-uart-debugger (target-name)
  "Запустить UART отладчик."
  (let* ((plist (mcu-debug-get-target-plist target-name))
         (uart-debugger (plist-get plist :uart-debugger))
         (uart-args (plist-get plist :uart-args))
         (project-dir (mcu-debug-find-project-root)))

    (unless uart-debugger
      (return-from mcu-debug-launch-uart-debugger nil))

    (let ((cmd (format "cd '%s' && %s %s &"
                       project-dir uart-debugger uart-args)))
      (message "Запуск UART отладчика: %s" cmd)
      (shell-command cmd)
      (sleep-for 3)
      t)))

(defun mcu-debug-stop-all-processes ()
  "Остановить все процессы MCU отладки."
  (interactive)
  ;; Останавливаем процессы simavr
  (shell-command "pkill -f simavr 2>/dev/null")
  ;; Останавливаем процессы avrdude
  (shell-command "pkill -f avrdude 2>/dev/null")
  ;; Закрываем GDB
  (when (get-buffer "*gud*")
    (kill-buffer "*gud*"))
  (message "Все процессы MCU отладки остановлены"))

;; ============================================================================
;; Построение команд GDB (ИСПРАВЛЕНО)
;; ============================================================================

(defun mcu-debug-build-gdb-init-script (elf-file sim-mode)
  "Создать скрипт инициализации для GDB."
  (let* ((port (if sim-mode 1234 mcu-debug-default-uart-port))
         (script-file (make-temp-file "gdb-init-" nil ".gdb"))
         (commands (format "file %s
target remote localhost:%d
break main
continue
" elf-file port)))

    (with-temp-file script-file
      (insert commands))

    script-file))

;; ============================================================================
;; Основные функции отладки (ИСПРАВЛЕНО)
;; ============================================================================

;;;###autoload
(defun mcu-debug-start-simulation (target-name)
  "Запустить отладку в симуляторе."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (elf-file (mcu-debug-find-elf-file project-dir target-name)))

    (unless (file-exists-p elf-file)
      (if (y-or-n-p (format "ELF файл %s не найден. Собрать проект?" (file-name-nondirectory elf-file)))
          (progn
            (compile "make -k all")
            (sleep-for 2)
            (unless (file-exists-p elf-file)
              (error "Проект не собран. Проверьте Makefile и компилятор")))
        (error "Отмена: ELF файл не найден")))

    (mcu-debug-stop-all-processes)

    (message "Запуск симуляции для %s..." target-name)

    ;; Запускаем симулятор
    (unless (mcu-debug-launch-simulator-simple elf-file)
      (error "Не удалось запустить симулятор. Убедитесь, что simavr установлен."))

    ;; Создаем скрипт инициализации GDB
    (let ((gdb-init-script (mcu-debug-build-gdb-init-script elf-file t))
          (gdb-exe (mcu-debug-ensure-gdb-executable target-name)))

      (message "Запуск GDB с инициализационным скриптом...")

      ;; Запускаем GDB в отдельном фрейме
      (let ((frame (make-frame `((name . ,(format "MCU-DEBUG-%s" target-name))
                                 (width . 200)
                                 (height . 85)
                                 (left . 1200)
                                 (top . 0)))))
        (select-frame frame)

        ;; Запускаем GDB с интерпретатором MI
        (gdb (format "%s --interpreter=mi -x %s" gdb-exe gdb-init-script))

        ;; Применяем раскладку окон
        (run-with-timer 2 nil
                        (lambda ()
                          (condition-case err
                              (when (fboundp 'my/apply-gdb-layout)
                                (my/apply-gdb-layout)
                                (message "Раскладка окон GDB применена"))
                            (error
                             (message "Не удалось применить раскладку GDB: %s" err)))))

        ;; Удаляем временный файл скрипта
        (run-with-timer 5 nil (lambda () (delete-file gdb-init-script)))

        (message "Сессия отладки симуляции запущена для %s" target-name)))))

;;;###autoload
(defun mcu-debug-start-uart-debug (target-name)
  "Запустить отладку через UART."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (elf-file (mcu-debug-find-elf-file project-dir target-name)))

    (unless (file-exists-p elf-file)
      (if (y-or-n-p (format "ELF файл %s не найден. Собрать проект?" (file-name-nondirectory elf-file)))
          (progn
            (compile "make -k all")
            (sleep-for 2)
            (unless (file-exists-p elf-file)
              (error "Проект не собран. Проверьте Makefile и компилятор")))
        (error "Отмена: ELF файл не найден")))

    (mcu-debug-stop-all-processes)

    (message "Запуск UART отладки для %s..." target-name)

    ;; Запускаем UART отладчик
    (unless (mcu-debug-launch-uart-debugger target-name)
      (message "UART отладчик не запущен. Убедитесь, что устройство подключено."))

    ;; Создаем скрипт инициализации GDB
    (let ((gdb-init-script (mcu-debug-build-gdb-init-script elf-file nil))
          (gdb-exe (mcu-debug-ensure-gdb-executable target-name)))

      (message "Запуск GDB с инициализационным скриптом...")

      ;; Запускаем GDB в отдельном фрейме
      (let ((frame (make-frame `((name . ,(format "MCU-DEBUG-%s" target-name))
                                 (width . 200)
                                 (height . 85)
                                 (left . 1200)
                                 (top . 0)))))
        (select-frame frame)

        ;; Запускаем GDB с интерпретатором MI
        (gdb (format "%s --interpreter=mi -x %s" gdb-exe gdb-init-script))

        ;; Применяем раскладку окон
        (run-with-timer 2 nil
                        (lambda ()
                          (condition-case err
                              (when (fboundp 'my/apply-gdb-layout)
                                (my/apply-gdb-layout)
                                (message "Раскладка окон GDB применена"))
                            (error
                             (message "Не удалось применить раскладку GDB: %s" err)))))

        ;; Удаляем временный файл скрипта
        (run-with-timer 5 nil (lambda () (delete-file gdb-init-script)))

        (message "Сессия отладки UART запущена для %s" target-name)))))

;; ============================================================================
;; Вспомогательные функции
;; ============================================================================

;;;###autoload
(defun mcu-debug-flash-firmware (target-name)
  "Прошить firmware в целевой микроконтроллер."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (plist (mcu-debug-get-target-plist target-name))
         (flash-target (plist-get plist :flash-make-target "flash")))

    (when (y-or-n-p "Собрать проект перед прошивкой?")
      (compile "make -k all"))

    (let ((cmd (format "make -C %s %s"
                       (shell-quote-argument project-dir)
                       flash-target)))
      (compile cmd))))

;;;###autoload
(defun mcu-debug-run-make-target ()
  "Выполнить цель Makefile для проекта."
  (interactive)

  (let* ((project-dir (mcu-debug-find-project-root))
         (targets (mcu-debug-get-makefile-targets project-dir))
         (make-target (completing-read "Цель Makefile: " targets nil t)))

    (let ((cmd (format "make -C %s %s"
                       (shell-quote-argument project-dir)
                       make-target)))
      (compile cmd))))

;; ============================================================================
;; Шаблоны и конфигурации
;; ============================================================================

;;;###autoload
(defun mcu-debug-generate-template (target-name)
  "Сгенерировать шаблон конфигурации для нового микроконтроллера."
  (interactive "sИмя микроконтроллера: ")

  (unless (file-exists-p mcu-debug-templates-dir)
    (make-directory mcu-debug-templates-dir t))

  (let ((template-file (expand-file-name
                        (format "%s.el" target-name)
                        mcu-debug-templates-dir)))
    (with-temp-file template-file
      (insert (format ";; Конфигурация для %s\n\n" target-name)
              "(mcu-debug-register-target\n"
              (format " \"%s\"\n" target-name)
              " '(:gdb-executable \"avr-gdb\"\n"
              "   :gdb-args \"\"\n"
              "   :simulator \"simavr\"\n"
              "   :simulator-args \"-m atmega328 -f 16000000 -g\"\n"
              "   :uart-debugger \"avrdude\"\n"
              "   :uart-args \"-c arduino -p atmega328p -P /dev/ttyACM0 -b 115200\"\n"
              "   :elf-suffix \".elf\"\n"
              "   :hex-suffix \".hex\"\n"
              "   :flash-make-target \"flash\"))\n\n")
              (format ";; Примеры использования:\n")
              (format ";; (mcu-debug-start-simulation \"%s\")\n" target-name)
              (format ";; (mcu-debug-start-uart-debug \"%s\")\n" target-name)
              (format ";; (mcu-debug-flash-firmware \"%s\")\n" target-name))

    (find-file template-file)
    (message "Шаблон создан: %s" template-file)))

;;;###autoload
(defun mcu-debug-load-all-templates ()
  "Загрузить все шаблоны из директории шаблонов."
  (interactive)
  (when (file-exists-p mcu-debug-templates-dir)
    (dolist (file (directory-files mcu-debug-templates-dir t "\\.el$"))
      (condition-case err
          (progn
            (load file nil 'nomessage)
            (message "Загружен шаблон: %s" (file-name-nondirectory file)))
        (error
         (message "Ошибка загрузки шаблона %s: %s"
                  (file-name-nondirectory file) err))))))

;; ============================================================================
;; Инициализация модуля и создание глобальных ключей
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
  (mcu-debug-load-all-templates)

  ;; Создаем свою карту
  (defvar mcu-debug-global-map (make-sparse-keymap)
    "Глобальная карта команд для отладки MCU.")

  ;; Основные команды отладки
  (define-key mcu-debug-global-map (kbd "s") 'mcu-debug-start-simulation)
  (define-key mcu-debug-global-map (kbd "u") 'mcu-debug-start-uart-debug)
  (define-key mcu-debug-global-map (kbd "f") 'mcu-debug-flash-firmware)
  (define-key mcu-debug-global-map (kbd "c") 'compile)
  (define-key mcu-debug-global-map (kbd "m") 'mcu-debug-run-make-target)
  (define-key mcu-debug-global-map (kbd "g") 'mcu-debug-generate-template)
  (define-key mcu-debug-global-map (kbd "l") 'mcu-debug-load-all-templates)
  (define-key mcu-debug-global-map (kbd "k") 'mcu-debug-stop-all-processes)

  ;; Информация о проекте
  (define-key mcu-debug-global-map (kbd "p")
    (lambda ()
      (interactive)
      (let ((project-dir (mcu-debug-find-project-root))
            (project-name (mcu-debug-get-project-name))
            (elf-files (directory-files
                        (expand-file-name "bin" project-dir)
                        t "\\.elf$")))
        (message "Проект: %s (%s)\nELF файлы: %s"
                 project-name project-dir
                 (if elf-files
                     (mapconcat 'file-name-nondirectory elf-files ", ")
                   "нет")))))

  ;; Привязываем к C-c g m
  (global-set-key (kbd "C-c g m") mcu-debug-global-map)

  (message "Модуль MCU Debugging инициализирован (%d целей)"
           (length mcu-debug-targets)))

;; Автоматическая инициализация при загрузке
(mcu-debug-initialize)

(provide 'mcu-debug)

;;; mcu-debug.el ends here
