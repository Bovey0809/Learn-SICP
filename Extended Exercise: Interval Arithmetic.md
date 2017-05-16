# Interval Arithmetic

## Questions

1. What is Interval Arithmetic?
  - The interval is some kind of data object
  - It represents the range of possible values of an inexact quantity

2. What's the like of the interval?

  - 2 endpoints

## Comprehense the question

### Background

We need a procedure to measure the qulity of products.
What we need is the precision value and the measured parameter.

#### An example of electrical engineer.
Compute the value of a parallel equavalent resistance R of two resisstors.
>resistence(R1, R2)  return R

The torlerance of the product.
Product has a tolerence which results in the range of a%.
So the value should between _value * (1-a)_ and _value * (1 +a)_
If the resistors are parrallel, the resistance calculated will be resulted in
a larger range.

Interval is a value with two endpoints, namely a value with tolerance.

Presumes of the **interval**, the interval can be manipulated with + - * /
And the result of the implementions are also the intervals.

'''
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
'''

The product of two intervals.

'''
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))
'''

- Why don't she just use p1 and p3 as the result of interval?
  - Isn't p1 p3 the min and max?
  - Do interval calculate something to change the value?

'''
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))
'''

## Exercises
### Exercise 2.7

Define selectors upper-bound and lower-bound

### Exercise 2.9

The width of an interval is the value half of upper and lower value

Operation of width is only implementing on intervals, but for others
the operation is not .

Write a function of the sum up the widths (only the intervals being add or
subtract)

Give examples for this is not true for multiplication and division.

What is the interval in the question?

Interval is one thing with two boundaries.

#### Math formulate

1. width

  - (high-value - low-value) / 2
2. sum-width

  - argument should be two intervals
  - width(interval1) + width(interval2)
3. Multiplication

  - width(interval1) * width(interval2)
  - What is the multiplication meaning in the question?
  - How can the width being multiplicated?

### Exercise 2.10

The divide procedure of interval has some error

When the interval become 0, which means (a, b) a=b

Then the divided by 0 is wrong.

Change the procedure of the **div-interval** throw an error when 0 comes

- an interval that spans zero
- 一个零件的跨度包含了0

如果这个零件的跨度包含了0， 那么除法会导致上下区间不一致。

所以要避免这种情况

题目中的跨越了原点的零件， 意味着有一个是负数，一个是正数
所以upper-bound 一定是正数， lower bound 一定是负数。

#### 数学模型
- 除法的函数涉及了两种情况
- 对于模型 1/x
  1. 当x>0，1/y1 < 1/y2
  2. 当x<0, 情况相反
  3. 另外x不能为0

- 对于 spans的情况
  - 和x<0的情况相同
#### 算法

经过思考其实我们只需要使用error写一个选择语句即可，不需要判断那么多。

注意抛出错误使用(error "The interval should not cantain 0")

### Exercise 2.11

Interval的乘法其实不需要那么多的计算，将其分为九种情况就可以减少运算量

Interval可以都为正数，一正一负，或者都为负数， 两个Interval相乘就有九种情况。

把九种情况都写出来就可以了

### 练习 2.12

定义一个程序，“make-center-percent”， 利用 _center_ 和 _tolerance_ 作为参数
返回 _interval_ 。

同时还需要定义一个选择器“percent”， 以“Interval”为参数，返回“tolerance”

除此之外还有一个选择器“center”， 返回“center”

#### 分析思路
make-center-percent 并不是 “cont” 因为他会返回一个 interval, 建立在现有Interval的基础上，
(make-intercal (center-tolerance) (center + tolerance))

center 的计算比较方便，把 (upper-bound + lower-bound) / 2 即可得到center 的值

tolerance的计算 (U-L)/(U+L)

假设这些数字都是正数。

### 练习 2.13

其实用中文写更有助于理解题目，毕竟翻译了一遍，过了脑子，英文写有点照抄的意思，以后用中文写博客吧。

对于并联系统（parallel），有一个可以近似计算系统容错率的方程，这个方程利用了两个零件（interval)的
容错率进行运算，得到系统容错率。

在这个问题中，容错率是正数。

高中时学的电阻并联计算公式，题目中的问题是，利用（div-interval)计算并联电阻，公式不同计算结果竟然不一样。

问题出在哪里？

#### 分析思路
猜想问题出在程序中的Interval是一个范围而不是一个固定值。
第一步应该重现错误，利用两个方程计算一个例子，再分析为何结果不同。

#### 数学模型
一步步分析下来，主要问题出在乘法上， 由于乘法选择（Exercise 2.11)九种方案中的最大值，在计算过程中
两种方法得到的结果不一样，第一种方法在最后一步才运用除法运算，第二个方法在一开始就应用了除法。

因此，得到的结果不同，但是哪种方法才是正确的呢？

第一种方法得到的范围比第二种方法要大，所以第二种方法可能更好一点。因为误差是由除法（乘法）
产生的，除法（乘法）的次数越多，误差越大。

尽可能的减少误差，因该使用第一种计算方式。

### 练习2.14
创建一些零件，尝试使用零件之间的除法， 例如A/A, A/B

对于2.13的题目，使用make-center-tolerance 的数据结构来尝试，
可以更方便的解释13题的区别。

#### 分析思路
转变零件的容忍度，查看并联系统的容忍度计算方法的变化。
也就是13题的延续。
 
