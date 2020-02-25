% author - Sarvansh Prasher
% version 1.0
% date - 13th February 2020

% full_words(Z) will define the number Z in words. 0-9 number
% will be defined first in word(N) which will work as helper in
% forming words.

word(0):- print(zero).
word(1):- print(one).
word(2):- print(two).
word(3):- print(three).
word(4):- print(four).
word(5):- print(five).
word(6):- print(six).
word(7):- print(seven).
word(8):- print(eight).
word(9):- print(nine).

full_words(Z):-Zmod is Z mod 10, Zdiv is Z // 10,digit(Zdiv),
    word(Zmod).

digit(0).

digit(Z):- Z > 0,Zmod is Z mod 10,
    Zdiv is Z // 10,
    digit(Zdiv),
    word(Zmod),
    print(-).
