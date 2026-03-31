;;; mcu-debug.el --- MCU Debugging Framework for Embedded Projects -*- lexical-binding: t -*-

;; Copyright (C) 2024 harkut
;; Author: harkut <yanovets.vasya@gmail.com>
;; Version: 1.8
;; Keywords: embedded, mcu, avr, arm, gdb, debug, release

;;; Commentary:
;; Модуль для отладки микроконтроллеров через GDB с поддержкой
;; AVR, ARM и других архитектур.

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
;; Базовые функции
;; ============================================================================

(defun mcu-debug-register-target (name config-plist)
  "Зарегистрировать новый целевой микроконтроллер."
  (let ((existing (assoc name mcu-debug-targets)))
    (if existing
        (setcdr existing config-plist)
      (push (cons name config-plist) mcu-debug-targets)))
  (message "Цель '%s' зарегистрирована" name))

(defun mcu-debug-get-target-plist (target-name)
  "Получить plist конфигурации целевого микроконтроллера."
  (let ((target (assoc target-name mcu-debug-targets)))
    (unless target
      (error "Целевой микроконтроллер '%s' не найден" target-name))
    (cdr target)))

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
     (:gdb-executable "gdb-multiarch"
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

;; Регистрируем встроенные цели
(dolist (target mcu-debug-builtin-targets)
  (let ((name (car target))
        (config (cadr target)))
    (mcu-debug-register-target name config)))

;; ============================================================================
;; Цели
;; ============================================================================

(mcu-debug-register-target
 "STM32F205"
 '(:gdb-executable "gdb-multiarch"
   :gdb-args ""
   :openocd "openocd"
   :openocd-args "-f scripts/openocd.cfg"
   :elf-suffix ".elf"
   :hex-suffix ".hex"
   :flash-make-target "flash"
   :debug-cflags "-g -Og -DDEBUG"
   :release-cflags "-O2 -DNDEBUG"
   :make-variables ((MCU . "STM32F205RET6")
                    (CC . "arm-none-eabi-gcc")
                    (OBJCOPY . "arm-none-eabi-objcopy")
                    (OBJDUMP . "arm-none-eabi-objdump")
                    (SIZE . "arm-none-eabi-size"))
   :memory-map ((flash :start 0x08000000 :size 0x100000)
                (ram   :start 0x20000000 :size 0x30000))))

(mcu-debug-register-target
 "STM32F446"
 '(:gdb-executable "gdb-multiarch"
   :gdb-args ""
   :uart-debugger "openocd"
   :uart-args "-f openocd.cfg"
   :elf-suffix ".elf"
   :hex-suffix ".hex"
   :flash-make-target "flash"
   :debug-cflags "-g -Og -DDEBUG"
   :release-cflags "-O2 -DNDEBUG"
   :make-variables ((MCU . "STM32F446RET6")
                    (CC . "arm-none-eabi-gcc")
                    (OBJCOPY . "arm-none-eabi-objcopy")
                    (OBJDUMP . "arm-none-eabi-objdump")
                    (SIZE . "arm-none-eabi-size"))
   :memory-map ((flash :start 0x08000000 :size 0x80000)
                (ram   :start 0x20000000 :size 0x20000))))

;; ============================================================================
;; Основные утилиты
;; ============================================================================

(defun mcu-debug-find-elf-file (project-dir target-name build-mode)
  "Найти ELF файл в директории проекта для указанного режима сборки."
  (let* ((target (assoc target-name mcu-debug-targets))
         (plist (cdr target))
         (elf-suffix (or (plist-get plist :elf-suffix) ".elf"))
         ;; Ищем ELF в поддиректориях по режиму сборки
         (debug-dir (expand-file-name "debug" (expand-file-name "bin" project-dir)))
         (release-dir (expand-file-name "release" (expand-file-name "bin" project-dir)))
         (elf-files '()))

    ;; Сначала ищем в режимной поддиректории
    (setq elf-files
          (cond
           ((string= build-mode "debug")
            (when (file-exists-p debug-dir)
              (directory-files debug-dir t (concat "\\" elf-suffix "$"))))
           ((string= build-mode "release")
            (when (file-exists-p release-dir)
              (directory-files release-dir t (concat "\\" elf-suffix "$"))))))

    ;; Если не нашли в поддиректориях, ищем в корне bin
    (when (null elf-files)
      (let ((bin-dir (expand-file-name "bin" project-dir)))
        (when (file-exists-p bin-dir)
          ;; Фильтруем файлы по имени, включая режим сборки
          (setq elf-files (directory-files bin-dir t (concat ".*"
                                                             (if (string= build-mode "debug")
                                                                 "-debug"
                                                               "-release")
                                                             "\\" elf-suffix "$"))))))

    ;; Если все еще не нашли, берем любой ELF файл
    (when (null elf-files)
      (let ((bin-dir (expand-file-name "bin" project-dir)))
        (when (file-exists-p bin-dir)
          (setq elf-files (directory-files bin-dir t (concat "\\" elf-suffix "$"))))))

    (cond
     ((= (length elf-files) 1) (car elf-files))
     ((> (length elf-files) 1)
      (completing-read "Выберите ELF файл: " elf-files nil t))
     (t (error "ELF файл не найден в %s. Соберите проект командой 'make %s' или 'make all'"
               (expand-file-name "bin" project-dir) build-mode)))))

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

(defun mcu-debug-ensure-gdb-executable (target-name)
  "Проверить доступность GDB для целевого микроконтроллера."
  (let* ((plist (mcu-debug-get-target-plist target-name))
         (gdb-exe (plist-get plist :gdb-executable)))
    (unless (executable-find gdb-exe)
      (error "GDB не найден: %s. Установите пакет с компилятором для этой архитектуры." gdb-exe))
    gdb-exe))

;; ============================================================================
;; Управление режимами сборки
;; ============================================================================

(defun mcu-debug-choose-build-mode ()
  "Выбрать режим сборки (debug или release)."
  (completing-read "Режим сборки: " '("debug" "release") nil t
                   mcu-debug-default-build-mode))

(defun mcu-debug-build-project (project-dir target-name build-mode)
  "Собрать проект в указанном режиме."
  (let* ((build-target (cond
                        ((string= build-mode "debug") "debug")
                        ((string= build-mode "release") "release")
                        (t "all"))))

    (message "Сборка проекта в режиме %s..." build-mode)

    (let ((cmd (format "make -C %s BUILD_MODE=%s %s"
                       (shell-quote-argument project-dir)
                       build-mode
                       build-target))
          (compilation-buffer-name "*MCU Build*"))

      (with-current-buffer (get-buffer-create compilation-buffer-name)
        (erase-buffer)
        (compilation-mode))

      (compile cmd)

      ;; Ждем завершения сборки
      (let ((proc (get-buffer-process compilation-buffer-name)))
        (when proc
          (while (eq (process-status proc) 'run)
            (sleep-for 0.1))))

      (message "Сборка завершена в режиме %s" build-mode))))

(defun mcu-debug-check-build-mode-support (project-dir)
  "Проверить поддержку режимов сборки в Makefile."
  (let ((makefile (expand-file-name "Makefile" project-dir))
        (has-debug nil)
        (has-release nil)
        (has-flash nil)
        (has-flash-release nil))

    (when (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (setq has-debug (re-search-forward "^debug:" nil t))
        (goto-char (point-min))
        (setq has-release (re-search-forward "^release:" nil t))
        (goto-char (point-min))
        (setq has-flash (re-search-forward "^flash:" nil t))
        (goto-char (point-min))
        (setq has-flash-release (re-search-forward "^flash-release:" nil t))))

    (list :debug has-debug
          :release has-release
          :flash has-flash
          :flash-release has-flash-release)))

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
    (let ((sim-buffer (get-buffer-create "*simavr*")))
      (with-current-buffer sim-buffer
        (erase-buffer)
        (compilation-shell-minor-mode 1)
        (setq-local comint-scroll-show-maximum-output t))
      (async-shell-command cmd sim-buffer)
      (sleep-for 3))
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
  "Запустить UART отладчик через make-process (без оболочки)."
  (let* ((plist (mcu-debug-get-target-plist target-name))
         (uart-debugger (plist-get plist :uart-debugger))
         (uart-args (plist-get plist :uart-args))
         (project-dir (expand-file-name (mcu-debug-find-project-root))))
    (if (not uart-debugger)
        (progn
          (message "UART отладчик не настроен для цели %s" target-name)
          nil)
      (let* ((cmd (concat uart-debugger " " uart-args))
             (proc-buffer (get-buffer-create "*uart-debugger*")))
        (message "Запуск UART отладчика: %s в %s" cmd project-dir)
        (with-current-buffer proc-buffer
          (erase-buffer)
          (compilation-shell-minor-mode 1))
        (make-process :name "openocd"
                      :buffer proc-buffer
                      :command (split-string-and-unquote cmd)
                      :directory project-dir
                      :connection-type 'pty)
        (sleep-for 5)
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
  (when (get-buffer "*MCU Build*")
    (kill-buffer "*MCU Build*"))
  (message "Все процессы MCU отладки остановлены"))

;; ============================================================================
;; Построение команд GDB
;; ============================================================================

(defun mcu-debug-build-gdb-init-script (project-dir elf-file sim-mode build-mode)
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
# Режим сборки: %s

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

# Предупреждение для release режима
%s
# Комментарии для пользователя:
# Используйте Ctrl+C для прерывания
# Ctrl+Z для паузы
# 'step', 'next', 'continue' для управления выполнением
" (current-time-string) build-mode
                          full-elf-path port full-project-path
                          (if (string= build-mode "release")
                              "echo \"ВНИМАНИЕ: Режим RELEASE. Отладочная информация ограничена.\""
                            ""))))

    (with-temp-file script-file
      (insert commands))

    (message "Создан скрипт GDB для режима %s: %s" build-mode script-file)
    script-file))

;; ============================================================================
;; Настройка окон GDB
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

(defun mcu-debug-apply-simple-gdb-layout ()
  "Упрощенная раскладка окон GDB без использования Purpose."
  (interactive)
  (delete-other-windows)

  ;; Делим фрейм на левую и правую колонки
  (let* ((left-win (selected-window))
         (right-win (split-window left-win (floor (* (window-total-width) 0.55)) 'right)))

    ;; ЛЕВАЯ КОЛОННА: исходный код и вывод GDB
    (with-selected-window left-win
      (let ((w-gdb (split-window-below (floor (* (window-height) 0.7)))))
        (with-selected-window w-gdb
          (let ((w-io (split-window-below (floor (* (window-height) 0.5)))))
            ;; Верхнее окно: исходный код
            (switch-to-buffer (other-buffer (current-buffer) t))
            ;; Среднее окно: вывод GDB
            (with-selected-window w-gdb
              (gdb-display-gdb-buffer))
            ;; Нижнее окно: ввод/вывод
            (with-selected-window w-io
              (gdb-display-io-buffer))))))

    ;; ПРАВАЯ КОЛОННА: отладочная информация
    (with-selected-window right-win
      (let* ((total-h (window-height))
             (h-step (floor (* total-h 0.1)))
             w-stack w-locals w-regs w-breaks)

        (setq w-stack (split-window-below h-step))
        ;; Верхнее окно: дизассемблирование
        (gdb-display-disassembly-buffer)

        (with-selected-window w-stack
          (setq w-locals (split-window-below h-step))
          ;; Второе окно: стек вызовов
          (gdb-display-stack-buffer))

        (with-selected-window w-locals
          (setq w-regs (split-window-below h-step))
          ;; Третье окно: локальные переменные
          (gdb-display-locals-buffer))

        (with-selected-window w-regs
          (setq w-breaks (split-window-below h-step))
          ;; Четвертое окно: регистры
          (gdb-display-registers-buffer))

        (with-selected-window w-breaks
          ;; Пятое окно: точки останова
          (gdb-display-breakpoints-buffer)))))

  (message "Упрощенная раскладка окон GDB применена"))

;; ============================================================================
;; Основные функции отладки
;; ============================================================================

(defun mcu-debug-launch-gdb-session (project-dir target-name sim-mode build-mode)
  "Запустить сессию GDB для отладки MCU."
  (let* ((elf-file (mcu-debug-find-elf-file project-dir target-name build-mode))
         (gdb-init-script (mcu-debug-build-gdb-init-script project-dir elf-file sim-mode build-mode))
         (gdb-exe (mcu-debug-ensure-gdb-executable target-name))
         (gdb-cmd (format "%s --interpreter=mi -x %s" gdb-exe gdb-init-script)))

    (message "Запуск GDB в режиме %s с командой: %s" build-mode gdb-cmd)

    ;; Создаем отдельный фрейм для отладки
    (let ((debug-frame (make-frame `((name . ,(format "MCU-DEBUG-%s-%s" target-name build-mode))
                                     (width . 200)
                                     (height . 85)
                                     (left . 1200)
                                     (top . 0)))))

      ;; Отключаем Purpose в новом фрейме полностью
      (with-selected-frame debug-frame
        (when (featurep 'purpose)
          (purpose-mode -1))  ; Выключаем purpose-mode в этом фрейме
        ;; Гарантируем, что helm и which-key работают
        (when (fboundp 'my/ensure-helm-in-frame)
          (my/ensure-helm-in-frame debug-frame))
        (when (fboundp 'my/ensure-which-key-in-frame)
          (my/ensure-which-key-in-frame debug-frame))
        ;; Устанавливаем правильную рабочую директорию
        (setq default-directory project-dir))

      ;; Запускаем GDB в правильной директории
      (let ((default-directory project-dir))
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
                              (compilation-shell-minor-mode 1)
                              (local-set-key (kbd "C-c C-c") 'gdb-gud-control-c)
                              (local-set-key (kbd "C-c C-z") 'gdb-gud-control-z)
                              (message "Буфер GDB настроен для интерактивного ввода (режим: %s)" build-mode))))))

      ;; Применяем упрощенную раскладку окон (без Purpose)
      (run-with-timer 3 nil
                      (lambda ()
                        (select-frame debug-frame)
                        (message "Применение упрощенной раскладки окон GDB...")
                        (condition-case err
                            (mcu-debug-apply-simple-gdb-layout)
                          (error
                           (message "Не удалось применить раскладку GDB: %s" err)))))

      ;; Удаляем временный файл скрипта через 10 секунд
      (run-with-timer 10 nil
                      (lambda ()
                        (when (file-exists-p gdb-init-script)
                          (delete-file gdb-init-script))))

      (message "Сессия GDB запущена для %s в режиме %s. Используйте буфер *gud* для команд."
               target-name build-mode))))

;;;###autoload
(defun mcu-debug-start-simulation (target-name)
  "Запустить отладку в симуляторе."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (build-mode (mcu-debug-choose-build-mode))
         (elf-file (mcu-debug-find-elf-file project-dir target-name build-mode)))

    (unless (file-exists-p elf-file)
      (if (y-or-n-p (format "ELF файл %s не найден в режиме %s. Собрать проект?"
                            (file-name-nondirectory elf-file) build-mode))
          (progn
            (mcu-debug-build-project project-dir target-name build-mode)
            (sleep-for 2)
            (unless (file-exists-p elf-file)
              (error "Проект не собран в режиме %s. Проверьте Makefile и компилятор" build-mode)))
        (error "Отмена: ELF файл не найден")))

    (mcu-debug-stop-all-processes)

    (message "Запуск симуляции для %s в режиме %s..." target-name build-mode)

    ;; Запускаем симулятор
    (unless (mcu-debug-launch-simulator-simple elf-file)
      (error "Не удалось запустить симулятор. Убедитесь, что simavr установлен."))

    ;; Запускаем GDB через 2 секунды
    (run-with-timer 2 nil (lambda ()
                            (mcu-debug-launch-gdb-session project-dir target-name t build-mode)))))

;;;###autoload
(defun mcu-debug-start-uart-debug (target-name)
  "Запустить отладку через UART."
  (interactive
   (list (completing-read "Целевой MCU: "
                          (mapcar 'car mcu-debug-targets)
                          nil t "AVR-ATMega328")))

  (let* ((project-dir (mcu-debug-find-project-root))
         (build-mode (mcu-debug-choose-build-mode))
         (elf-file (mcu-debug-find-elf-file project-dir target-name build-mode)))

    (unless (file-exists-p elf-file)
      (if (y-or-n-p (format "ELF файл %s не найден в режиме %s. Собрать проект?"
                            (file-name-nondirectory elf-file) build-mode))
          (progn
            (mcu-debug-build-project project-dir target-name build-mode)
            (sleep-for 2)
            (unless (file-exists-p elf-file)
              (error "Проект не собран в режиме %s. Проверьте Makefile и компилятор" build-mode)))
        (error "Отмена: ELF файл не найден")))

    (mcu-debug-stop-all-processes)

    (message "Запуск UART отладки для %s в режиме %s..." target-name build-mode)

    ;; Запускаем UART отладчик
    (unless (mcu-debug-launch-uart-debugger target-name)
      (message "UART отладчик не запущен. Убедитесь, что устройство подключено."))

    ;; Запускаем GDB через 3 секунды
    (run-with-timer 3 nil (lambda ()
                            (mcu-debug-launch-gdb-session project-dir target-name nil build-mode)))))

;; ============================================================================
;; Вспомогательные функции
;; ============================================================================

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
(defun mcu-debug-build-project-target (target-name build-mode)
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
      (mcu-debug-build-project-target target-name build-mode))

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
