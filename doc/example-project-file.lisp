; project files have the following syntax:
; ---------------------------------------
; SEMICOLON ';' denotes a line comment
; OPEN-PAREN '(' and CLOSE-PAREN ')' are used to delimit lists
; SYMBOLS are alphanumerics [a-z|A-Z|0-9|-] (this is the minimal SPEC,
; for the full SPEC see: CLTL2)
; SYMBOLS are delimited by Whitespace or OPEN-PAREN/CLOSE-PAREN that belong
; to a list
; STRINGS are delimited by DOUBLE-QUOTES '"' ("[^"]*") and may contain 
; only ASCII characters. UTF8 characters can be used in their escaped form,
; as well (see: CLTL2 for a full SPEC of Strings)
; dotted lists can be used, but may be converted to proper lists
; NIL elements are ignored
; empty lists are ignored
;  
; File Format:
; ------------
; a project file is an association list, that means it stores 
; key-value pairs in an arbitrary order
; keys may be case-sensitive, so please use the keys as indicated below
; Key: project-name
;  Value:  the name of the project, must be non-empty
; Key: java-version
;  Value: the version of Java used to create the project,
;         may be important as different run-time libraries may have been used
;         to compile the project
; Key: project-directory
;  Value: the directory-name relative to the repository-directory,
;         where the project is stored
; Key: project-library-directory
;  Value: the directory-name relative to the repository-directory,
;         of the directory where all .jar files with libraries needed by the
;         project are stored (excluding the Java run-time libraries,
;         because these are strongly coupled to the JVM used for the analysis)
; Key: project-analysis-directory
;  Value: the directory-name relative to the repository-directory,
;         of the directory where the jana metamodel representation of the
;         project's classes and aspects is stored
; Key: project-compilation-directory
;  Value: the directory-name relative to the repository-directory,
;         of the directory where the class files of the transformed analysis
;         results will be stored
; Key: project-transformation-directory
;  Value: the directory-name relative to the repository-directory,
;         of the directory where the jimple intermediate representation of
;         the transformed jana metamodel is stored
; Key: project-jar-file
;  Value: the name-String of the jar file where the classes
;         of the project are stored
; Key: project-final-jar-file
;  Value: the name-String of the jar file where the classes
;         of the project are stored after transformation of the metamodel
;         and compilation of the intermediate representation
; Key: project-classname-dictionary-file
;  Value: the name-String of the file where the mapping between
;         class-names and the files that store the jana-metamodel
;         representation of the named classes.
;         This file is an association list, and filenames are
;         relative to the repository-directory.
; Key: project-library-jar-files
;  Value: a classpath containing the filenames of all library .jar files
;         in the project-library-directory, which the project depends upon.
;         all filenames in the classpath are relative to the
;         repository-directory.
; Key: project-aspects
;  Value: Strings containing the fully qualified Java-Names of classes
;         that have aspect annotations
; Key: project-classes
;  Value: Strings containing the fully qualified Java-Names of classes
;         in the project that belong to the base-code and have no aspect
;         annotations.
; Key: project-transformed-classes
;  Value: Strings containing the fully qualified Java-Names of all classes
;         in the project after the metamodel has been transformed.
;         This information is needed, because the transformation may add or
;         remove classes. Before transformation, this value is empty. 
;
; Example:
; --------
(project
 (project-name "jana-examples")
 (java-version "Java Version: 1.6.0 from IBM Corporation")
 (project-directory "project-jana-examples")
 (project-library-directory "project-jana-examples/lib")
 (project-analysis-directory "project-jana-examples/lsp")
 (project-compilation-directory "project-jana-examples/bin")
 (project-transformation-directory "project-jana-examples/trn")
 (project-jar-file "project-jana-examples/jana-examples.jar")
 (project-final-jar-file "project-jana-examples/jana-examples-final.jar")
 (project-classname-dictionary-file "project-jana-examples/jana-examples.cnd")
 (project-library-jar-files "project-jana-examples/lib/bin.jar")
 (project-aspects "example.jana.classes.TestAspect")
 (project-classes "example.jana.classes.ArrayExample"
   "example.jana.classes.SuperClassExample"
   "example.jana.classes.Month"
   "example.jana.classes.InnerClassExample$InnerClass2$InnerInnerClass"
   "example.jana.classes.InnerClassExample"
   "example.jana.classes.Pointcut"
   "example.jana.classes.ArithmeticExample"
   "example.jana.classes.Day"
   "example.jana.classes.InnerInterfaceTest$InnerInterface"
   "example.jana.classes.TestInterface"
   "example.jana.classes.MonthTest"
   "example.jana.classes.SwitchStatementExample"
   "example.jana.classes.SynchronizationExample"
   "example.jana.classes.InnerInterfaceTest"
   "example.jana.classes.IfExample"
   "example.jana.classes.Month$MonthGER"
   "example.jana.classes.ConstantTypes"
   "example.jana.classes.InheritanceExample"
   "example.jana.classes.ObjectExample"
   "example.jana.classes.Month$MonthUK"
   "example.jana.classes.ExceptionExample"
   "example.jana.classes.InnerClassExample$1"
   "example.jana.classes.InnerClassExample$InnerClass2")
 (project-transformed-classes))
