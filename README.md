* CONCEPTO
Quiero hacer un modo de Emacs para editar en Java totalmente independiente de cualquier IDE, simplemente a través de elisp (o usando algo más pero lo menos posible, para evitar
complicaciones innecesarias)
* TODO IDEAS REALIZABLES
- Autocompletado usando "jar tf (.jar)" y luego con javap -cp "el jar" "una de las clases que hayamos visto con el jar tf"
- Un archivo auxiliar en la carpeta del proyecto que liste los jar que usamos para crear el classpath directamente y ayudarnos en el autocompletado
- Derivar el proyecto de java-mode para mantener el color
* IDEAS PENDIENTES DE CHECK
- Crear un árbol con las clases del proyecto y demás, que puedas ver si interactúan entre ellas, qué son interfaces...
- Un debugger (cosa difícil)
- Un flycheck con la sintaxis de Java
