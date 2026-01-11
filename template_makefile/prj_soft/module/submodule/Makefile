# ==========================================
# Настройки проекта
# ==========================================
TARGET   = ex_1
CC       = gcc
SRC_DIR  = src
OBJ_DIR  = obj
INC_DIR  = include
DOC_DIR  = doc

# Режим сборки по умолчанию
BUILD_MODE ?= debug

# Поиск исходников
SRCS     = $(wildcard $(SRC_DIR)/*.c)
OBJS     = $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(SRCS))
DEPS     = $(OBJS:.o=.d)

# Docker настройки
IMAGE_NAME = harkut/$(TARGET)
DOCKERFILE = Dockerfile

# ==========================================
# Флаги компиляции и линковки
# ==========================================

# Общие флаги (зависимости -MMD -MP)
COMMON_CFLAGS = -Wall -Wextra -I$(INC_DIR) -MMD -MP

ifeq ($(BUILD_MODE), release)
    CFLAGS  = $(COMMON_CFLAGS) -O3
    LDFLAGS = -s
else
    # -g3 для макросов в GDB, -O0 для точной отладки в Emacs
    CFLAGS  = $(COMMON_CFLAGS) -g3 -O0 -DDEBUG=1
    LDFLAGS =
endif

# Библиотеки
LDLIBS    = #-lm -lpthread

# Инструменты анализа
CHECKER  = cppcheck
VALGRIND = valgrind
DOXYGEN  = doxygen

# ==========================================
# Основные цели (Rules)
# ==========================================

.PHONY: all clean run check memcheck debug doc docker-build docker-run

all: $(TARGET)

# Линковка
$(TARGET): $(OBJS)
	@echo "--- Linking ($(BUILD_MODE)): $@ ---"
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

# Компиляция
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	@echo "--- Compiling: $< ---"
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Подключение файлов зависимостей (.d)
-include $(DEPS)

# ==========================================
# Служебные команды
# ==========================================

# Очистка
clean:
	@echo "Cleaning up..."
	rm -rf $(OBJ_DIR) $(TARGET) $(DOC_DIR)

# Запуск
run: $(TARGET)
	./$(TARGET)

# Статический анализ
check:
	@echo "Running Cppcheck..."
	$(CHECKER) --enable=all --suppress=missingIncludeSystem -I$(INC_DIR)\
		$(SRC_DIR)

# Проверка памяти (Valgrind)
memcheck: $(TARGET)
	@echo "Running Valgrind..."
	$(VALGRIND) --leak-check=full --show-leak-kinds=all --track-origins=yes\
		./$(TARGET)

# Быстрый вызов отладчика
debug: $(TARGET)
	gdb ./$(TARGET)

# ==========================================
# Документация и Docker
# ==========================================

# Генерация документации
doc:
	@echo "--- Generating Doxygen Documentation ---"
	@if [ -f Doxyfile ]; then \
		mkdir -p $(DOC_DIR); \
		$(DOXYGEN) Doxyfile; \
		if [ -d "$(DOC_DIR)/latex" ]; then \
			echo "--- Generating PDF (LaTeX) ---"; \
			$(MAKE) -C $(DOC_DIR)/latex; \
			cp $(DOC_DIR)/latex/refman.pdf \
				$(DOC_DIR)/$(TARGET)_docs.pdf; \
			echo "PDF created: $(DOC_DIR)/$(TARGET)_docs.pdf"; \
		fi; \
	else \
		echo "Error: Doxyfile not found."; \
	fi

# Сборка Docker образа
docker-build:
	@echo "--- Building Docker Image: $(IMAGE_NAME) ---"
	docker build -t $(IMAGE_NAME) -f $(DOCKERFILE) .

# Запуск в контейнере
docker-run:
	@echo "--- Running in Docker ---"
	docker run --rm -it $(IMAGE_NAME)
