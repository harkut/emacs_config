# @	Имя цели
# $<	Первая зависимость
# $^	Все зависимости
# $?	Зависимости новее цели	(если изменились)
# $*	Stem (часть до %)
# $(@D)	Директория цели
# $(<F)	Имя файла зависимости
# ==========================================
# Настройки проекта
# ==========================================
TARGET   = avr_main
MCU      = atmega328p
CC       = avr-gcc
F_CPU    = 16000000
SRC_DIR  = src
OBJ_DIR  = obj
BIN_DIR  = bin
INC_DIR  = include
DOC_DIR  = doc

# Режим сборки (debug/release) - можно передать извне
BUILD_MODE ?= debug

# Создаем поддиректории для разных режимов сборки
OBJ_DIR_MODE = $(OBJ_DIR)/$(BUILD_MODE)
BIN_DIR_MODE = $(BIN_DIR)/$(BUILD_MODE)

# Поиск исходников
SRCS     = $(wildcard $(SRC_DIR)/*.c)
OBJS     = $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR_MODE)/%.o, $(SRCS))
DEPS     = $(OBJS:.o=.d)

# ==========================================
# Утилиты
# ==========================================
OBJCOPY  = avr-objcopy
OBJDUMP  = avr-objdump
SIZE     = avr-size
CHECKER  = cppcheck
DOXYGEN  = doxygen
AVRDUDE  = avrdude

# ==========================================
# Настройки программатора
# ==========================================
# Для Arduino Uno/Nano
PROGRAMMER_TYPE = arduino
AUTO_PORT := $(shell ls /dev/ttyACM* /dev/ttyUSB* 2>/dev/null | head -n1)
ifeq ($(AUTO_PORT),)
    PROGRAMMER_PORT = /dev/ttyACM0
else
    PROGRAMMER_PORT = $(AUTO_PORT)
endif
PROGRAMMER_BAUD = 57600

# ==========================================
# Флаги компиляции и линковки в зависимости от режима
# ==========================================
# DEBUG режим: с отладочной информацией и минимальными оптимизациями
# -Og: оптимизация для отладки (не нарушает отладочную информацию)
# -g: отладочная информация
# -DDEBUG: макрос для отладочного кода
DEBUG_CFLAGS   = -g -Og -DDEBUG

# RELEASE режим: максимальная оптимизация по размеру, без отладочной информации
# -Os: оптимизация по размеру
# -flto: межмодульная оптимизация
# -DNDEBUG: отключает assert и отладочный код
RELEASE_CFLAGS = -Os -flto -DNDEBUG

# Выбор флагов в зависимости от режима
ifeq ($(BUILD_MODE),debug)
    CFLAGS_MODE = $(DEBUG_CFLAGS)
    TARGET_SUFFIX = -debug
    # В debug режиме отключаем некоторые предупреждения для delay.h
    WARN_FLAGS = -Wall -Wextra -Wno-error=cpp
else ifeq ($(BUILD_MODE),release)
    CFLAGS_MODE = $(RELEASE_CFLAGS)
    TARGET_SUFFIX = -release
    # В release режиме все предупреждения = ошибки
    WARN_FLAGS = -Wall -Wextra -Werror
else
    $(error Неизвестный режим сборки: $(BUILD_MODE). Используйте debug или release)
endif

# Базовые флаги компиляции
# -mmcu=$(MCU) - целевой микроконтроллер
# -DF_CPU=$(F_CPU)UL - define для макросов задержки
# -ffunction-sections -fdata-sections - раздельная компиляция для линкера,
#					каждая функция в отдельной секции →
#						линкер может удалить неиспользуемые
# -funsigned-char -funsigned-bitfields - строгая типизация явная
# -fpack-struct -fshort-enums - оптимизация структур
# -std=gnu99 - стандарт C с расширениями GNU
# -I$(INC_DIR) - путь к заголовкам
BASE_CFLAGS   = -mmcu=$(MCU) -DF_CPU=$(F_CPU)UL \
                $(WARN_FLAGS) \
                -ffunction-sections -fdata-sections \
                -funsigned-char -funsigned-bitfields \
                -fpack-struct -fshort-enums \
                -std=gnu99 \
                -I$(INC_DIR)

CFLAGS   = $(BASE_CFLAGS) $(CFLAGS_MODE)

# -Wl,-Map,file.map - генерация карты памяти (анализ распределения памяти)
# -Wl,--gc-sections - удаление неиспользуемых секций (уменьшение размера)
# -Wl,--relax - релаксация (оптимизация переходов для AVR)
LDFLAGS  = -mmcu=$(MCU) \
           -Wl,--gc-sections \
           -Wl,--relax

# Для release режима добавляем межмодульную оптимизацию
ifeq ($(BUILD_MODE),release)
    LDFLAGS += -flto
endif

# ==========================================
# Основные цели (Rules)
# ==========================================
.PHONY: all clean doc flash flash-release check help debug release $(OBJ_DIR_MODE) $(BIN_DIR_MODE)

# all → зависит от debug (по умолчанию)
all: debug

# Специальные цели для режимов сборки
debug: BUILD_MODE = debug
debug: $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).hex size

release: BUILD_MODE = release
release: $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).hex size

# Создание директорий
$(OBJ_DIR_MODE) $(BIN_DIR_MODE):
	@mkdir -p $@

# Генерация hex файла
# -O ihex - формат Intel HEX
# -R .eeprom - исключение секции EEPROM
$(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).hex: $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).elf
	@echo "Creating HEX file for $(BUILD_MODE)..."
	$(OBJCOPY) -O ihex -R .eeprom $< $@
	@echo "HEX file created: $@"

# Линковка
$(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).elf: $(OBJS) | $(BIN_DIR_MODE)
	@echo "Linking for $(BUILD_MODE)..."
	$(CC) $(LDFLAGS) -o $@ $(OBJS)
	@echo "ELF created: $@"

# Компиляция с генерацией зависимостей
# % - wildcard (подстановка)
# $< - исходный файл (.c)
# -MMD: Создать файл зависимостей (исключая системные заголовки)
# -MP: Добавить phony-таргеты для заголовков
# -MF: Указать имя файла зависимостей
# -MT: Указать имя цели
# -c - только компиляция (без линковки)
$(OBJ_DIR_MODE)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR_MODE)
	@echo "Compiling $< for $(BUILD_MODE)..."
	$(CC) $(CFLAGS) -MMD -MP -MF $(@:.o=.d) -c $< -o $@

# Подключение файлов зависимостей (.d)
-include $(DEPS)

# ==========================================
# Программирование
# ==========================================
#     -p $(MCU) - тип микроконтроллера
#     -c $(PROGRAMMER_TYPE) - тип программатора
#     -P $(PORT) - порт подключения
#     -b $(BAUD) - скорость
#     -U flash:w:file.hex:i - операция записи
# Формат операции: -U <memtype>:r|w|v:<filename>[:format]
#     memtype - flash, eeprom, hfuse, lfuse, efuse
#     r - read, w - write, v 	- verify
#     :i - формат Intel HEX
flash: debug
	@echo "Programming $(MCU) (debug mode)..."
	$(AVRDUDE) -p $(MCU) -c $(PROGRAMMER_TYPE) \
	  -P $(PROGRAMMER_PORT) -b $(PROGRAMMER_BAUD) \
	  -U flash:w:$(BIN_DIR)/debug/$(TARGET)-debug.hex:i

flash-release: release
	@echo "Programming $(MCU) (release mode)..."
	$(AVRDUDE) -p $(MCU) -c $(PROGRAMMER_TYPE) \
	  -P $(PROGRAMMER_PORT) -b $(PROGRAMMER_BAUD) \
	  -U flash:w:$(BIN_DIR)/release/$(TARGET)-release.hex:i

# Для самодельных программаторов
isp:
	@echo "Programming via ISP..."
	$(AVRDUDE) -p $(MCU) -c usbasp \
	  -U flash:w:$(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).hex:i

# Установка фьюзов (пример для ATmega328P с внутренним RC 8MHz)
fuses:
	@echo "Setting fuses for $(MCU)..."
	$(AVRDUDE) -p $(MCU) -c $(PROGRAMMER_TYPE) \
	  -P $(PROGRAMMER_PORT) -b $(PROGRAMMER_BAUD) \
	  -U lfuse:w:0xe2:m -U hfuse:w:0xd9:m -U efuse:w:0xff:m

# ==========================================
# Анализ и отладка
# ==========================================
size: $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).elf
	@echo "Memory usage for $(BUILD_MODE):"
	$(SIZE) --format=avr --mcu=$(MCU) $<

# -h - заголовки секций
# -S - перемешивание с исходным кодом (если есть отладочная информация)
disasm: $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).elf
	@echo "Generating disassembly for $(BUILD_MODE)..."
	$(OBJDUMP) -h -S $< > $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).lst
	@echo "Disassembly saved to $(BIN_DIR_MODE)/$(TARGET)$(TARGET_SUFFIX).lst"

# ==========================================
# Очистка
# ==========================================
clean:
	@echo "Cleaning up..."
	rm -rf $(OBJ_DIR) $(BIN_DIR) $(DOC_DIR) *.map *.lst

# Полная очистка (включая бэкапы)
distclean: clean
	@find . -name "*~" -o -name "*.bak" -o -name "*.backup" | xargs rm -f

# Очистка только определенного режима
clean-debug:
	@echo "Cleaning debug build..."
	rm -rf $(OBJ_DIR)/debug $(BIN_DIR)/debug

clean-release:
	@echo "Cleaning release build..."
	rm -rf $(OBJ_DIR)/release $(BIN_DIR)/release

# ==========================================
# Статический анализ
# ==========================================
check:
	@echo "Running static analysis..."
	$(CHECKER) --enable=all \
	  --suppress=missingIncludeSystem \
	  -I$(INC_DIR) \
	  $(SRC_DIR)/*.c

# ==========================================
# Документация
# ==========================================
doc:
	@echo "Generating documentation..."
	@mkdir -p $(DOC_DIR)
	@if [ -f Doxyfile ]; then \
		$(DOXYGEN) Doxyfile; \
		if [ -d "$(DOC_DIR)/latex" ]; then \
			echo "Generating PDF..."; \
			$(MAKE) -C $(DOC_DIR)/latex >/dev/null 2>&1; \
			cp $(DOC_DIR)/latex/refman.pdf \
				$(DOC_DIR)/$(TARGET)_documentation.pdf; \
			echo "PDF created: $(DOC_DIR)/$(TARGET)_documentation.pdf"; \
		fi; \
	else \
		echo "Warning: Doxyfile not found. Using default config..."; \
		$(DOXYGEN) -g Doxyfile >/dev/null 2>&1; \
		sed -i 's/^PROJECT_NAME.*/PROJECT_NAME = "$(TARGET)"/' Doxyfile; \
		sed -i 's|^OUTPUT_DIRECTORY.*|OUTPUT_DIRECTORY = $(DOC_DIR)|' Doxyfile; \
		sed -i 's/^INPUT.*/INPUT = $(SRC_DIR) $(INC_DIR)/' Doxyfile; \
		$(DOXYGEN) Doxyfile; \
	fi

# ==========================================
# Справка
# ==========================================
help:
	@echo "AVR Bare-metal Makefile с поддержкой debug/release режимов"
	@echo "=========================================================="
	@echo "Доступные цели:"
	@echo "  all        : Собрать проект в debug режиме (по умолчанию)"
	@echo "  debug      : Собрать проект с отладочной информацией (-g -Og)"
	@echo "  release    : Собрать проект с оптимизацией (-Os -flto)"
	@echo "  clean      : Удалить все артефакты сборки"
	@echo "  clean-debug: Удалить только debug сборку"
	@echo "  clean-release: Удалить только release сборку"
	@echo "  distclean  : Полная очистка + удаление бэкапов"
	@echo "  flash      : Прошить MCU (debug режим)"
	@echo "  flash-release: Прошить MCU (release режим)"
	@echo "  isp        : Прошить через ISP программатор"
	@echo "  fuses      : Установить фьюзы (настройте сначала!)"
	@echo "  check      : Статический анализ кода"
	@echo "  doc        : Генерировать документацию"
	@echo "  size       : Показать использование памяти"
	@echo "  disasm     : Генерировать листинг дизассемблирования"
	@echo "  help       : Показать эту справку"
	@echo ""
	@echo "Конфигурация:"
	@echo "  MCU        : $(MCU)"
	@echo "  F_CPU      : $(F_CPU)"
	@echo "  SRC_DIR    : $(SRC_DIR)"
	@echo "  INC_DIR    : $(INC_DIR)"
	@echo "  BUILD_MODE : $(BUILD_MODE) (можно переопределить: make BUILD_MODE=release)"
	@echo "  DEBUG_FLAGS: $(DEBUG_CFLAGS)"
	@echo "  RELEASE_FLAGS: $(RELEASE_CFLAGS)"
	@echo ""
	@echo "Примеры использования:"
	@echo "  make debug              # Сборка с отладочной информацией"
	@echo "  make release            # Сборка с оптимизацией"
	@echo "  make BUILD_MODE=release # Альтернативный способ"
	@echo "  make flash-release      # Прошить release версию"
