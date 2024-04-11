CC = g++
CFLAGS = -std=c++17 -O3
TARGET = main

define compile_all
	$(CC) $(CFLAGS) $(TARGET).cpp -o $(TARGET) $(1)
	./$(TARGET)
endef

linreg:
	$(call compile_all, )

basic:
	$(call compile_all, -DBASIC)

clean:
	$(RM) $(TARGET)