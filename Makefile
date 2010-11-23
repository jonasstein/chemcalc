default: prog

SRC = read

%.f95:
f95 -o $(SRC).f95 $<

run:
./$(SRC) > werteliste.dat

prog:
f95 $(SRC).f95 -o $(SRC)

save:
git commit -a
git push
