![hangTheLampions](http://i.imgur.com/sgASX81.png)

#Background

This project started out as an experiment to learn Scala in general, and Parser Combinators in particular. It has been developed and refined over the course of 3 years between 2012 and 2015.

Please note that I never intended this project to be read or modified by somebody else besides me. In the beginning, I wasn't even sure I could get the project off the ground in a timely fashion; or at all. There are no class comments or method comments. There are, however, some interesting implementation comments and TODOs for possible future improvements.

The project has been stale for quite some time now, and I don't see myself putting any more effort into improving Karel. It has turned out more than good enough for the purpose it was intended for; teaching students the fundamental control structures of imperative programming.

#Thanks

- Richard Pattis, for inventing [Karel the Robot](http://en.wikipedia.org/wiki/Karel_%28programming_language%29)
- Martin Odersky, for inventing [Scala](http://www.scala-lang.org/)
- Robert Futrell, for providing [RSyntaxTextArea](https://github.com/bobbylight/RSyntaxTextArea)

#Eclipse setup

##Scala plugin
```
Help -> Install New Software...

Work with:
http://download.scala-ide.org/sdk/lithium/e44/scala211/stable/site
(Press Enter)
[x] Scala IDE for Eclipse
[ ] Contact all update sites during install to find required software
(This would take ages)
Next

Next

(o) I accept the terms of the license agreements
Finish
(Installing Software, takes some time)

Yes
(Eclipse restarts)
Never
```
##Scala Perspective
```
Window -> Open Perspective -> Other...
Scala
OK
```
##Git import
```
File -> Import...

Git -> Projects from Git
Next

Clone URI
Next

Location URI:
https://github.com/fredoverflow/karel.git
Next

Next
Next
Next
Finish
```
##Run
```
Right-click on Main.scala inside package gui

Run as -> Scala Application
```
Click the Start button and then step through the program with F12,
or accelerate the process by moving the speed slider to the right.

#Keyboard shortcuts
```
Simple Statements
F1    moveForward();
F2    turnLeft();
F3    turnAround();
F4    turnRight();
F5    pickBeeper();
F6    dropBeeper();

Simple Conditions
F7    onBeeper()
F8    beeperAhead()
F9    leftIsClear()
F10   frontIsClear()
F11   rightIsClear()
```
Conditions can be combined with the usual suspects `!`, `&&` and `||`.
```
F12   start / step into / reset

Ctrl Space     auto-complete
Ctrl Shift I   auto-indent
Ctrl Shift R   visualize call stack
Ctrl Shift M   show virtual machine
Ctrl Shift T   optimize tail calls
Ctrl Shift D   run static analysis
```
#Control Structures
```
repeat (number)
{
    ...
}

if (condition)
{
    ...
}

if (condition)
{
    ...
}
else
{
    ...
}

while (condition)
{
    ...
}
```
#How do I declare variables?
You can't :) Karel has no notion of types and variables.

#How do I save my code?
The code is automatically saved inside a karel folder in your home directory.
