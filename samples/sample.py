x = input()
cnt = int(input())
y = ""
i = 0
while i < cnt:
    y = x + str(i) + y
    print("iter ", i, ": ", y, "\n")
    i = i + 1
print("done.\n")
