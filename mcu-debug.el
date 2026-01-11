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
;; Запуск симуляторов и отладчиков
;; ============================================================================

(defun mcu-debug-launch-simulator-simple (elf-file)
  "Простой запуск simavr в терминале."
  (let* ((elf-dir (file-name-directory elf-file))
         (elf-name (file-name-nondirectory elf-file))
         (cmd (format "cd '%s' && simavr -m atmega328 -f 16000000 -g '%s'"
                      elf-dir elf-name)))

    (message "Запуск simavr командой: %s" cmd)

    ;; Запускаем в отдельном буфере
    (let ((sim-buffer (get-buffer-create "*simavr*")))
      (with-current-buffer sim-buffer
        (erase-buffer)
        (compilation-shell-minor-mode 1)
        (setq-local comint-scroll-show-maximum-output t))

      (async-shell-command cmd sim-buffer)
      (sleep-for 3))

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

    (let ((cmd (format "cd '%s' && %s %s"
                       project-dir uart-debugger uart-args)))
      (message "Запуск UART отладчика: %s" cmd)

      (let ((uart-buffer (get-buffer-create "*uart-debugger*")))
        (with-current-buffer uart-buffer
          (erase-buffer)
          (compilation-shell-minor-mode 1))

        (async-shell-command cmd uart-buffer)
        (sleep-for 3)
        t))))

(defun mcu-debug-stop-all-processes ()
  "Остановить все процессы MCU отладки."
  (interactive)
  ;; Останавливаем процессы simavr
  (shell-command "pkill -f simavr 2>/dev/null")
  ;; Останавливаем процессы avrdude
  (shell-command "pkill -f avrdude 2>/dev/null")
  ;; Останавливаем процессы openocd
  (shell-command "pkill -f openocd 2>/dev/null")
  ;; Закрываем GDB буферы
  (when (get-buffer "*gud*")
    (kill-buffer "*gud*"))
  (message "Все процессы MCU отладки остановлены"))

;; ============================================================================
;; Построение команд GDB
;; ============================================================================

(defun mcu-debug-build-gdb-init-script (project-dir elf-file sim-mode)
  "Создать скрипт инициализации для GDB в директории проекта."
  (let* ((port (if sim-mode 1234 mcu-debug-default-uart-port))
         ;; Создаем скрипт в директории проекта
         (script-file (expand-file-name ".gdbinit-debug" project-dir))
         ;; Полные пути к файлам
         (full-elf-path (expand-file-name elf-file))
         (full-project-path project-dir)
         (commands (format "
# Автоматический скрипт инициализации GDB для MCU отладки
# Создан %s

# Устанавливаем параметры
set confirm off
set pagination off
set height 0
set width 0

# Подключаемся к цели
file \"%s\"
target remote localhost:%d
directory \"%s\"

# Устанавливаем точку останова на main
break main

# Комментарии для пользователя:
# Используйте Ctrl+C для прерывания
# Ctrl+Z для паузы
# 'step', 'next', 'continue' для управления выполнением
" (current-time-string) full-elf-path port full-project-path)))

    (with-temp-file script-file
      (insert commands))

    (message "Создан скрипт GDB: %s" script-file)
    script-file))

;; ============================================================================
;; Настройка окон GDB (аналогичная my/apply-gdb-layout из init.el)
;; ============================================================================

(defun mcu-debug-get-gdb-buffer-by-mode (mode)
  "Поиск буфера GDB по Major Mode."
  (cl-find-if (lambda (b) (with-current-buffer b (eq major-mode mode)))
              (buffer-list)))

(defun mcu-debug-populate-gdb-windows ()
  "Заполнение окон строго по их назначению."
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
             (buf (mcu-debug-get-gdb-buffer-by-mode mode))
             (win (cl-find-if (lambda (w)
                                (and (window-live-p w)
                                     (fboundp 'purpose-window-purpose-get)
                                     (eq (purpose-window-purpose-get w) purp)))
                              (window-list))))
        (when (and buf win (window-live-p win))
          (set-window-buffer win buf)
          (set-window-dedicated-p win t))))))

(defun mcu-debug-apply-gdb-layout ()
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

    (run-with-timer 0.5 nil #'mcu-debug-populate-gdb-windows)))

;; ============================================================================
;; Основные функции отладки
;; ============================================================================

(defun mcu-debug-launch-gdb-session (project-dir target-name sim-mode)
  "Запустить сессию GDB для отладки MCU."
  (let* ((elf-file (mcu-debug-find-elf-file project-dir target-name))
         (gdb-init-script (mcu-debug-build-gdb-init-script project-dir elf-file sim-mode))
         (gdb-exe (mcu-debug-ensure-gdb-executable target-name))
         (gdb-cmd (format "%s --interpreter=mi -x %s" gdb-exe gdb-init-script)))

    (message "Запуск GDB с командой: %s" gdb-cmd)

    ;; Создаем отдельный фрейм для отладки
    (let ((debug-frame (make-frame `((name . ,(format "MCU-DEBUG-%s" target-name))
                                     (width . 200)
                                     (height . 85)
                                     (left . 1200)
                                     (top . 0)))))
      (select-frame debug-frame)

      ;; Включаем Purpose в новом фрейме
      (with-selected-frame debug-frame
        (unless purpose-mode
          (purpose-mode 1))
        ;; Гарантируем, что helm и which-key работают
        (my/ensure-helm-in-frame debug-frame)
        (my/ensure-which-key-in-frame debug-frame))

      ;; Устанавливаем рабочую директорию и запускаем GDB
      (let ((default-directory project-dir))
        ;; Запускаем GDB
        (gdb gdb-cmd))

      ;; Настраиваем буфер GDB для интерактивности
      (run-with-timer 1 nil
                      (lambda ()
                        (let ((gdb-buffer (get-buffer "*gud*")))
                          (when (buffer-live-p gdb-buffer)
                            (with-current-buffer gdb-buffer
                              (setq buffer-read-only nil)
                              (setq-local comint-scroll-show-maximum-output t)
                              (setq-local comint-prompt-read-only nil)
                              (setq-local gud-minor-mode 'gdbmi)
                              ;; Включаем режим компиляции для интерактивных команд
                              (compilation-shell-minor-mode 1)
                              ;; Настраиваем клавиши
                              (local-set-key (kbd "C-c C-c") 'gdb-gud-control-c)
                              (local-set-key (kbd "C-c C-z") 'gdb-gud-control-z)
                              (message "Буфер GDB настроен для интерактивного ввода"))))))

      ;; Применяем раскладку окон через 3 секунды, чтобы GDB успел создать буферы
      (run-with-timer 3 nil
                      (lambda ()
                        (select-frame debug-frame)
                        (message "Применение раскладки окон GDB...")
                        (condition-case err
                            (progn
                              (mcu-debug-apply-gdb-layout)
                              (message "Раскладка окон GDB применена"))
                          (error
                           (message "Не удалось применить раскладку GDB: %s" err)))))

      ;; Удаляем временный файл скрипта через 10 секунд
      (run-with-timer 10 nil
                      (lambda ()
                        (when (file-exists-p gdb-init-script)
                          (delete-file gdb-init-script))))

      (message "Сессия GDB запущена для %s. Используйте буфер *gud* для команд." target-name))))

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

    ;; Запускаем GDB через 2 секунды
    (run-with-timer 2 nil (lambda ()
                            (mcu-debug-launch-gdb-session project-dir target-name t)))))

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

    ;; Запускаем GDB через 3 секунды
    (run-with-timer 3 nil (lambda ()
                            (mcu-debug-launch-gdb-session project-dir target-name nil)))))

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
;; Утилиты для работы с буфером GDB
;; ============================================================================

(defun my/unlock-gdb-buffer ()
  "Разблокировать буфер GDB для ввода команд."
  (interactive)
  (let ((gdb-buffer (get-buffer "*gud*")))
    (when (buffer-live-p gdb-buffer)
      (with-current-buffer gdb-buffer
        (setq buffer-read-only nil)
        (setq-local comint-scroll-show-maximum-output t)
        (setq-local comint-prompt-read-only nil)
        (message "Буфер GDB разблокирован для ввода"))
      t)))

(defun my/gdb-send-command (command)
  "Отправить команду в буфер GDB."
  (interactive "sКоманда GDB: ")
  (let ((gdb-buffer (get-buffer "*gud*")))
    (when (buffer-live-p gdb-buffer)
      (with-current-buffer gdb-buffer
        (goto-char (point-max))
        (insert command)
        (comint-send-input))
      (message "Команда отправлена: %s" command))))

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

  ;; Утилиты для работы с GDB
  (define-key mcu-debug-global-map (kbd "U") 'my/unlock-gdb-buffer)
  (define-key mcu-debug-global-map (kbd "C") 'my/gdb-send-command)

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
