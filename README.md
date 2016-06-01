* CONCEPT

Our goal is to make an Emacs mode to edit in Java using only Elisp.

* REASONABLE IDEAS

- Auto-complete using "jar tf [.jar]" command and "javap -cp [jar] [jar class]
- An auxiliar archive in the project folder to list used JARs, in order to create directly our classpath
- Derivate project from java-mode

* NOT-SO-REASONABLE IDEAS

- Create a tree with all project classes, showing relations and types of each one
- Debugger
- A Java flycheck-like mode
