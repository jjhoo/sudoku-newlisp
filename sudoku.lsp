;; -*- newlisp -*-

;; Copyright (c) 2017 Jani J. Hakala <jjhakala@gmail.com> Tampere, Finland
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as
;;  published by the Free Software Foundation, version 3 of the
;;  License.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(context 'scand)

(define (box-number-to-index num)
  (case num
    (1 (cons 1 1)) (2 (cons 1 2)) (3 (cons 1 3))
    (4 (cons 2 1)) (5 (cons 2 2)) (6 (cons 2 3))
    (7 (cons 3 1)) (8 (cons 3 2)) (9 (cons 3 3))
    (true "invalid")))

;; rather stupid way but...
(define (num-to-box-number num)
  (case num
    (1 1) (2 1) (3 1)
    (4 2) (5 2) (6 2)
    (7 3) (8 3) (9 3)))

(define (cell-box-calc row col)
  (cons (num-to-box-number row) (num-to-box-number col)))
;;  (println row ", " col " -> " (cons (+ (/ (- row 1) 3) 1) (+ (/ (- col 1) 3) 1)))
;; (+ 1 (+ (* (/ (- row 1) 3) 3) (/ (- col 1) 3))))

(define (cell-pos cell)
  (first cell))

(define (cell-value cell)
  (last cell))

(define (cell-init row col n)
  (cons (list row col (cell-box-calc row col)) n))

(define (cell-box cell)
  (nth 2 (first cell)))

(define (cell-col cell)
  (nth 1 (first cell)))

(define (cell-row cell)
  (nth 0 (first cell)))

(define (match-pos pos)
  (expand '(pos ?) 'pos))

(define (match-box box)
  (expand '((? ? box) ?) 'box))

(define (match-col col)
  (expand '((? col (? ?)) ?) 'col))

(define (match-row row)
  (expand '((row ? (? ?)) ?) 'row))

(define (init)
  (let (res '())
    (for (i 1 9)
      (for (j 1 9)
        (for (n 1 9)
          (push (cell-init i j n) res -1))))
    res))

(define (get-box box cands)
  (if (integer? box)
      (filter (curry match (match-box (box-number-to-index box))) cands)
      (filter (curry match (match-box box)) cands)))
;; (filter (curry (fn (b cell) (= b (cell-box cell))) box) cands))
;; (filter (curry match (expand '((? ? box) ?) 'box)) cands))

(define (get-cell pos cands)
  (filter (curry match (match-pos pos)) cands))

(define (get-col col cands)
  (filter (curry match (match-col col)) cands))

(define (get-row row cands)
  (filter (curry match (match-row row)) cands))

(define (same-box? pos1 pos2)
  (= (nth 2 pos1) (nth 2 pos2)))

(define (same-col? pos1 pos2)
  (= (nth 1 pos1) (nth 1 pos2)))

(define (same-row? pos1 pos2)
  (= (nth 0 pos1) (nth 0 pos2)))

(define (same-val? cell1 cell2)
  (= (nth 1 cell1) (nth 1 cell2)))

(define (sees? pos1 pos2)
  (or (same-box? pos1 pos2)
      (same-col? pos1 pos2)
      (same-row? pos1 pos2)))

(define (cell-sees? cell1 cell2)
  (sees? (nth 0 cell1) (nth 0 cell2)))

(define (numbers pos cands)
  (map (fn (cell) (nth 1 cell))
       (filter (fn (cell) (= pos (nth 0 cell))) cands)))

(context MAIN)
(context 'sudoku)

(define (sudoku:str-to-solved s)
  (let (i 1 j 1 solved '())
    (dolist (c (explode s))
      (if (!= (int c) 0)
          (push (scand:cell-init i j (int c)) solved -1))
      ;;            (println "(" i ", " j ") - " (int c))))

      (if (= j 9)
          (silent (inc i) (set 'j 1))
          (inc j)))
    solved))

;; could probably be more effective; don't bother for now
(define (init solved)
  (let (cands (scand:init))
    (dolist (cell1 solved)
    ;; (clean (fn (cell) (= 0 (scand:cell-value cell))) solved))
      (let (pos1 (scand:cell-pos cell1))
        (set 'cands
             (clean
              (fn (cell2)
                (let (pos2 (scand:cell-pos cell2))
                  (or (= pos1 pos2) ;; cell is solved
                      (and (scand:sees? pos1 pos2)
                           (scand:same-val? cell1 cell2))))) cands)))
      cands)))

(define (finder pred cands)
  (let (found '() funs2 (list scand:get-row scand:get-col scand:get-box))
    (for (i 1 9)
      (dolist (fun2 funs2)
        (set 'found (append found (pred (fun2 i cands))))))
    (unique (sort found))))

(define (unique-positions cands)
  (unique (map (fn (cell) (nth 0 cell)) cands)))

;; Find unsolved cells that have only one candidate left
(define (find-singles-simple solved cands)
  (letn (fun (fn (cands)
               (let (nsolved '())
                 (dolist (pos (unique-positions cands))
                   (let (row (nth 0 pos) col (nth 1 pos) box (nth 2 pos))
                     (let (ncands (filter (fn (cell)
                                            (= (first cell) pos)) cands))
                       (if (= (length ncands) 1)
                           (push (first ncands) nsolved -1)))))
                 nsolved))
             res (finder fun cands))
    (list res '())))

(define (cell-numbers cell cands)
  (map (curry nth 1) (scand:get-cell (nth 0 cell) cands)))

(define (cell-numbers-for-pos pos cands)
  (map (curry nth 1) (scand:get-cell pos cands)))

(define (numbers cands)
  (sort (map (curry nth 1) cands)))

(define (unique-numbers cands)
  (unique (numbers cands)))

(define (number-counts-cond pred numbers)
  (letn ((unums (unique numbers))
         (counts (map (fn (n)
                        (list n (length (filter (curry = n) numbers))))
                      unums)))
    (filter (fn (item) (pred (nth 1 item))) counts)))

(define (number-counts numbers limit)
  (letn ((unums (unique numbers))
         (counts (map (fn (n)
                        (list n (length (filter (curry = n) numbers))))
                      unums)))
    (if (nil? limit)
        counts
        (filter (fn (item) (= limit (nth 1 item))) counts))))

(define (unique-positions cands)
  (unique (sort (map (curry nth 0) cands))))

;; Knuth, algorithm T
(define (combinations-t k multiset)
  (let ((cjs (map (fn (j) (- j 1)) (sequence 0 k)))
        (j k)
        (continue true)
        (res '()))
    (push (length multiset) cjs -1)
    (push 0 cjs -1)

    (while continue
      ;; (println "cjs << " (rest cjs) " >> j << " j)
      (set
       'continue
       (catch
           (begin
             ;; visit: Visit the combination, in this case push to res
             ;; (println "T2: j " j ", k " k ", x " x ", cjs " cjs)
             (push (map (fn (i) (nth i multiset))
                        (slice cjs 1 k))
                   res -1)
             (if (> j 0)
                 (silent ;; T6
                  ;; (println "T6")
                  (set 'x j)
                  (setf (nth j cjs) x)
                  (-- j)
                  (throw true)))
             ;; (println "T3: j=" j ", x=" x ", cjs=" cjs)
             (if (< (+ (nth 1 cjs) 1) (nth 2 cjs))
                 (begin
                   (setf (nth 1 cjs) (+ (nth 1 cjs) 1))
                   (throw true))
                 (set 'j 2))
             ;; (println "T4: j=" j ", x=" x ", cjs=" cjs)
             (let (cont true)
               (while cont
                 (setf (nth (- j 1) cjs) (- j 2))
                 (set 'x (+ (nth j cjs) 1))
                 (if (= x (nth (+ j 1) cjs))
                     (++ j)
                     (set 'cont nil))))
             ;; (println "T5: j " j ", k " k ", x " x ", cjs " cjs)
             (if (> j k)
                 (throw nil))
             ;; T6
             ;; (println "T6: j " j ", k " k ", x " x ", cjs " cjs)
             (setf (nth j cjs) x)
             (-- j)
             true))))
    res))

(define (combinations k items)
  (if (<= (length items) k) (list items)
      (= k 1) (map list items)
      (combinations-t k items)))

;; Find unsolved cells that appear only once in a set
(define (find-singles solved cands)
  (letn (fun (fn (cands)
               (let (nsolved '())
                 (dolist (n (unique-numbers cands))
                   (let (ncands (filter (fn (cell) (= n (nth 1 cell))) cands))
                     (if (= 1 (length ncands))
                         (set 'nsolved (append nsolved ncands)))))
                 (unique (sort nsolved))))
             res (finder fun cands))
    (list res '())))

(define (find-naked-groups-in-set limit cands)
  (if (< (length (unique-positions cands)) (+ limit 1))
      (throw '()))
  (letn ((found '())
         (nums (numbers cands))
         (ncounts (number-counts nums))
         (unums (unique (sort (map (curry nth 0) ncounts))))
         (combs (combinations limit unums)))
    ;; now find if 'limit' count of cells contain a naked group
      ;; of 'limit' count of numbers
    ;; (println "Combos (" limit ") >> " combs " << for cands >> " cands)
    (dolist (nums combs)
      (letn ((foos (find-cells-cond (fn (numbers)
                                      (or (= nums numbers)
                                          (empty? (difference numbers nums))))
                                    cands)))
        (if (= (length foos) limit)
            (letn ((positions (map (curry nth 0) foos)))
              ;; (println "Found something? " nums " " foos)
              (set 'found (append found
                                  (filter (fn (cell)
                                            (and (find (scand:cell-value cell)
                                                       nums)
                                                 (not (find (scand:cell-pos cell)
                                                            positions))))
                                          cands)))))))
    found))

(define (find-naked-groups limit solved cands)
  (println "Naked group (" limit ")")
  (letn ((found '())
         (res (finder (fn (cands)
                        (catch (find-naked-groups-in-set limit cands)))
                      cands)))
    (list '() (unique (sort res)))))

(define (find-hidden-groups-in-set limit cands)
  (if (< (length (unique-positions cands)) (+ limit 1))
      (throw '()))
  ;; (println "Set is (" limit ") " cands)
  (letn ((found '())
         (nums (numbers cands))
         (ncounts (number-counts-cond (fn (len)
                                        (and (>= len 2)
                                             (<= len limit)))
                                      nums))
         (unums (unique (sort (map (curry nth 0) ncounts)))))
    ;; (println "Unums " unums ", ncounts " ncounts)
    (if (< (length unums) limit)
        (throw '()))

    ;; now find if a limited number of cells contain a certain hidden group
    (let (combs (combinations limit unums))
      ;; (println "Combos (" limit ") >> " combs " << for cands >> " cands)
      (dolist (nums combs)
        (letn ((ncands (clean (fn (cell)
                                (not (find (scand:cell-value cell) nums)))
                              cands))
               (positions (unique-positions ncands))
               (cells (map (fn (pos)
                             (list pos
                                   (cell-numbers-for-pos pos ncands)))
                           positions)))
          ;; (println "asdf limit " limit ", cells " cells)
          (if (= limit (length cells))
              (begin
                ;; (println "Found hidden group? " nums
                ;;          ", positions " positions
                ;;          "\n\t cands " cands)
                (set 'found (append
                             found
                             (filter (fn (cell)
                                       (and (not (find (scand:cell-value cell)
                                                       nums))
                                            (find (scand:cell-pos cell)
                                                  positions)))
                                     cands))))))))
    found))
              ;; (let (cell-nums (unique-numbers ncands))
              ;;   (println "Cells for combo (" limit ")\n\t" nums "\n\t" cell-nums "\n\t" cells "\n\t" cands)
              ;;   ;; (println "Too many cells: nums " nums ", cells " cells)
              ;;   ))))))
  ;; '())

(define (find-hidden-groups limit solved cands)
  (println "Hidden group (" limit ")")
  (letn ((found '())
         (res (finder (fn (cands)
                        (catch (find-hidden-groups-in-set limit cands)))
                      cands)))
    (list '() res)))

(define (cells-in-col? xs)
  (letn ((x0 (first xs))
         (col (scand:cell-col x0)))
    (catch
        (dolist (cell (rest xs))
          (if (!= col (scand:cell-col cell))
              (throw nil))
          true))))

(define (cells-in-row? xs)
  (letn ((x0 (first xs))
         (row (scand:cell-row x0)))
    (catch
        (dolist (cell (rest xs))
          (if (!= row (scand:cell-row cell))
              (throw nil))
          true))))

(define (cells-in-line? xs)
  (or (cells-in-row? xs) (cells-in-col? xs)))

;; 2 or 3 candidates of a number in the same line and box,
;; no other candidates for the same box
;; --> other candidates in the same line can be eliminated
(define (find-pointing-pairs solved cands)
  (println "Pointing pairs")
  (let (found '())
    (for (b 1 9)
      (catch
          (letn ((box (scand:get-box b cands))
                 (nums (unique-numbers box)))
            (if (< (length box) 2)
                (throw 'next))
            (dolist (n nums)
              (let (xs (filter (fn (cell)
                                 (= n (scand:cell-value cell)))
                               box))
                (if (and (or (= (length xs) 2)
                             (= (length xs) 3))
                         (cells-in-line? xs))
                    (letn ((ys (if (cells-in-row? xs)
                                   (scand:get-row (scand:cell-row (first xs))
                                                  cands)
                                   (scand:get-col (scand:cell-col (first xs))
                                                  cands)))
                           (positions (map (curry nth 0) xs))
                           (nfound (filter (fn (cell)
                                             (and (= n (scand:cell-value cell))
                                                  (not (find (scand:cell-pos cell)
                                                             positions))))
                                           ys)))
                      (set 'found (append found nfound)))))))))
    (list '() found)))

(define (cells-in-same-box cells)
  (let (cell0 (first cells))
    (set 'res
         (catch
             (begin
               (if (empty? (rest cells)) (throw nil))
               (dolist (cell (rest cells))
                 (if (= nil (scand:same-box? (scand:cell-pos cell0)
                                             (scand:cell-pos cell)))
                     (throw nil)))
               (throw true))))
    res))

;; 2 or 3 candidates of a number in the same line and box,
;; no other candidates for the same line
;; --> other candidates in the same box can be eliminated
(define (find-box/line-reduction solved cands)
  (println "Box/line reduction")
  (letn ((found '())
         (setfun
          (fn (cset cands)
            (letn ((fun (fn (n) (filter (fn (cell)
                                          (= n (scand:cell-value cell)))
                                        cset)))
                   (num-occurrances (clean (fn (n-cells)
                                             (empty? (nth 1 n-cells)))
                                           (map (fn (n) (list n (fun n)))
                                                (sequence 1 9))))
                   (interesting (filter
                                 (fn (n-cells)
                                   (let (len (length (nth 1 n-cells)))
                                     (and
                                      (or (= len 2) (= len 3))
                                      (cells-in-same-box (nth 1 n-cells)))))
                                 num-occurrances))
                   (results (map (fn (foo)
                                   (letn ((n (nth 0 foo))
                                          (cells (nth 1 foo))
                                          (b (nth 2 (scand:cell-pos (first cells))))
                                          (box-cells (scand:get-box b cands))
                                          (other (filter
                                                  (fn (cell)
                                                    (and
                                                     (= n (scand:cell-value cell))
                                                     (not (find cell cells))))
                                                  box-cells)))
                                     other)) interesting)))
              ;; (println "Foos " num-occurrances)
              ;; (println "Interesting " interesting)
              ;; (println "    IN " cset)
              ;; (println "    results " (flat results 1))
              (flat results 1)))))
    (for (row 1 9)
      (letn ((row-set (scand:get-row row cands))
             (xs (setfun row-set cands)))
        (if (not (empty? xs))
            (set 'found (append found xs)))))
    (for (col 1 9)
      (letn ((col-set (scand:get-col col cands))
             (xs (setfun col-set cands)))
        (if (not (empty? xs))
            (set 'found (append found xs)))))
    (list '() found)))

(define (find-cells-cond pred cands)
  (letn ((poss (unique-positions cands))
         (pos-nums (map (fn (pos)
                          (list pos (cell-numbers-for-pos pos cands)))
                        poss)))
    (filter (fn (pos-num-p) (pred (nth 1 pos-num-p))) pos-nums)))

(define (find-cells-with-n-candidates limit cands)
  (find-cells-cond (fn (numbers) (= limit (length numbers))) cands))

(define (is-ywing-numbers-comb? pivot winga wingb)
  (= (nth 1 pivot) (difference (union (nth 1 winga)
                                      (nth 1 wingb))
                               (intersect (nth 1 winga)
                                          (nth 1 wingb)))))

(define (is-ywing? pivot wing1 wing2)
  (catch
      (if (not (and (scand:sees? (nth 0 pivot) (nth 0 wing1))
                    (scand:sees? (nth 0 pivot) (nth 0 wing2))))
          (throw nil)
          (!= 3 (length (unique (flat (map (curry nth 1)
                                           (list pivot wing1 wing2)) 1))))
          (throw nil)
          (is-ywing-numbers-comb? pivot wing1 wing2))))

(define (all-p pred xs)
  (catch
      (dolist (x xs)
        (if (not (pred x))
            (throw nil))))
  true)

(define (any-p pred xs)
  (catch
      (dolist (x xs)
        (if (pred x)
            (throw true))))
  nil)

;; three cells wing1-pivot-wing2
;; with values  AC-AB-BC
;; -> C eliminated from cells seen by wing1 and wing2
(define (find-ywing solved cands)
  (println "Y-wing")
  (letn
      ((found '())
       (pairs (find-cells-with-n-candidates 2 cands))
       (good-comb-fun
        (fn (comb)
          (letn (numbers (sort (unique (union (flat (map (curry nth 1)
                                                         comb) 1)))))
            (and (= 3 (length numbers))
                 (all-p (fn (n) (= 2 (length (filter (fn (n-cell)
                                                       (find n (nth 1 n-cell)))
                                                     comb))))
                        numbers)))))
       (combs (filter good-comb-fun (combinations 3 pairs))))
    ;; (println "Pairs " pairs)
    ;; (println "Combinations for " (length pairs)
    ;;           " pairs, filtered combinations " (length combs))
    ;; (println "    " combs)
    (dolist (comb combs)
      (dolist (perm (map (fn (c) (map (fn (i) (nth i comb)) c))
                         ;; just listing 6 possible permutations might
                         ;; not be good style.
                         (list (list 0 1 2) (list 0 2 1)
                               (list 1 0 2) (list 1 2 0)
                               (list 2 0 1) (list 2 1 0))))
        (let ((wing1 (nth 0 perm))
              (pivot (nth 1 perm))
              (wing2 (nth 2 perm)))
          (if (is-ywing? pivot wing1 wing2)
              (letn ((n (first (intersect (nth 1 wing1) (nth 1 wing2))))
                     (poss (map (curry nth 0) (list pivot wing1 wing2)))
                     (xs (filter (fn (cell)
                                   (let (pos (scand:cell-pos cell))
                                     (and (= n (scand:cell-value cell))
                                          (not (find pos poss))
                                          (scand:sees? (nth 0 wing1) pos)
                                          (scand:sees? (nth 0 wing2) pos))))
                                 cands)))
                (if (not (empty? xs))
                    (set 'found (append found xs)))))
          ;; (println "Is a y-wing perm! " wing1 " - " pivot " - " wing2)
          ;; (println "   maybe eliminate (" n ") << " xs " >> of << " cands)
          ;; (println " c       " perm)))
          )))
    (list '() found)))

(define (update-solved found solved)
  (sort (union solved found)))

(define (update-candidates-solved solved cands)
  (let (ncands (copy cands))
    (dolist (cell1 solved)
      (set 'ncands (clean (fn (cell2)
                             (or (= (scand:cell-pos cell1)
                                    (scand:cell-pos cell2))
                                 (and
                                  (= (scand:cell-value cell1)
                                     (scand:cell-value cell2))
                                  (scand:cell-sees? cell1 cell2))))
                           ncands)))
    ;; (println "Foo " solved " - " (difference cands ncands true))
    ncands))

(define (update-candidates removals cands)
  (difference cands removals true))

(define (solver solved cands)
  (letn ((funs (list find-singles-simple
                     find-singles
                     (fn (solved cands) (find-naked-groups 2 solved cands))
                     (fn (solved cands) (find-naked-groups 3 solved cands))
                     (fn (solved cands) (find-hidden-groups 2 solved cands))
                     (fn (solved cands) (find-hidden-groups 3 solved cands))
                     (fn (solved cands) (find-naked-groups 4 solved cands))
                     (fn (solved cands) (find-hidden-groups 4 solved cands))
                     find-pointing-pairs
                     find-box/line-reduction
                     find-ywing))
         (continue true)
         (fun (fn ()
                (dolist (f funs)
                  (set 'osolved (copy solved))
                  (set 'ocands (copy cands))
                  (let (res (f solved cands))
                    (println "res 1 " res)
                    (if (not (empty? (nth 0 res)))
                        (silent
                         (set 'solved (update-solved (nth 0 res) solved))
                         (set 'cands (update-candidates-solved (nth 0 res)
                                                               cands))
                         (println "Solved cells 1 "
                                  (difference solved osolved))
                         (println "Removed candidates 1 "
                                  (difference ocands cands))
                         (throw (not (empty? cands)))))
                    (println "res 2")
                    (if (not (empty? (nth 1 res)))
                        (silent
                         (set 'cands (update-candidates (nth 1 res) cands))
                         (println "Removed candidates 2 "
                                  (difference ocands cands))
                         (throw true))))))))
    (while (= continue true)
      ;; (println "Continue 1 " solved " -- " cands)
      (if (catch (fun))
          true
          (empty? cands)
          (begin
            (println "Solved? " (length solved))
            (set 'continue nil))
          (begin
            (println "Do not continue") ;; no progress
            (println "    " cands)
            (set 'continue nil))))))

;; (set 'GRID "000040705500780120170502006815407960467008051009615478950873010781264539002050007")
;; This has box/line reduction, simple colouring and y-wing
;; (set 'GRID "000040700500780020070002006810007900460000051009600078900800010080064009002050000")
;; needs naked triple, y-wing
(set 'GRID "014600300050000007090840100000400800600050009007009000008016030300000010009008570")
;; can proceed with hidden pair
;; (set 'GRID "000000000904607000076804100309701080008000300050308702007502610000403208000000000")
;; naked triple, hidden triple
;; (set 'GRID "300000000970010000600583000200000900500621003008000005000435002000090056000000001")
(set 'solved (str-to-solved GRID))
;; (println solved)
(set 'cands (sudoku:init solved))
;; (println SOLVED)
;; (println cands)
(solver solved cands)
;; (println (scand:get-row 1 solved))
;; (println (scand:get-col 1 solved))
;; (println (scand:get-box 1 solved))

(context MAIN)
