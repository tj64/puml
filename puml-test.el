;;; puml-test.el --- ERT Tests for puml.el (PlantUML DSL)

;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.9
;; URL: https://github.com/tj64/puml

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  0.9
;;   :licence:  GPL 3 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :keywords: emacs org-mode org-bandbook plantuml ERT
;;   :git-repo: https://github.com/tj64/puml
;;   :git-clone: git://github.com/tj64/puml.git
;;   :END:

;;;; Commentary

;; Library puml.el implements a domain-specific-language (DSL) for
;; PlantUML in Emacs Lisp. This file contains ERT tests for this DSL,
;; using the examples from the PlantUML 'Language Reference Guide"
;; (http://plantuml.sourceforge.net/sources.html)

;;; Requires 

(require 'puml)

;;; Variables 
;;; Tests

;;;; Wrappers
;;;; Common
;;;; Sequence Diagram
;;;;; Basic examples
;;;;; Declaring participant
;;;;; Use non-letters in participants 
;;;;; Message to Self
;;;;; Message sequence numbering
;;;;; Title
;;;;; Splitting diagrams
;;;;; Grouping message
;;;;; Notes on messages
;;;;; Some other notes
;;;;; Formatting using HTML
;;;;; Divider
;;;;; Lifeline Activation and Destruction
;;;;; Participant creation
;;;;; Incoming and outgoing messages
;;;;; Stereotypes and Spots
;;;;; More information on titles
;;;;; Participants englober
;;;;; Removing Footer
;;;;; Skinparam
;;;;; Skin

;;;; Usecase Diagram

;;;;; Usecases

;; Use cases are enclosed using between parentheses (because two
;; parentheses looks like an oval).

;; You can also use the usecase keyword to define a usecase. And you
;; can define an alias, using the as keyword. This alias will be used
;; latter, when defining relations.

;; @startuml                       
                                
;; (First usecase)                 
;; (Another usecase) as (UC2)      
;; usecase UC3                     
;; usecase (Last\nusecase) as UC4  
                                
;; @enduml                         

;;;;; Actors

;; Actor are enclosed using between two points.

;; You can also use the actor keyword to define an actor. And you can
;; define an alias, using the as keyword. This alias will be used
;; latter, when defining relations.

;; We will see latter than actor definitions is optional.

;; @startuml                   
                            
;; :First Actor:               
;; :Another\nactor: as Men2    
;; actor Men3                  
;; actor :Last actor: as Men4  
                            
;; @enduml                     

;;;;; Usecases description

;; If you want to have description on several lines, you can use
;; quotes.

;; You can also use the following separators: -- .. == __. And you can
;; put titles within the separators.

@startuml                              
                                       
usecase UC1 as "You can use            
several lines to define your usecase.  
You can also use separators.           
--                                     
Several separators are possible.       
==                                     
And you can add titles:                
..Conclusion..                         
This allows large description."        
                                       
@enduml                                


#+header: :file ex1.png
#+begin_src plantuml                              
                                       
usecase UC1 as "You can use            
several lines to define your usecase.  
You can also use separators.           
--                                     
Several separators are possible.       
==                                     
And you can add titles:                
..Conclusion..                         
This allows large description."        
                                       
#+end_src


;;;;; Basic example

;; To link actors and use cases, the arrow --> is used.

;; The more dashes "-" in the arrow, the longer the arrow. You can add
;; a label on the arrow, by adding a ":" character in the arrow
;; definition.

;; In this example, you see that User has not been defined before, and
;; is used as an actor.

;; @startuml                                                              
                                                                       
;; User -> (Start)                                                        
;; User --> (Use the application) : A small label                         
                                                                       
;; :Main Admin: ---> (Use the application) : This is\nyet another\nlabel  
                                                                       
;; @enduml                                                                

;;;;; Extension

;; If one actor/use case extends another one, you can use the symbol <|
;; -- (which stands for ) .
;; As for smiley, when you turn your head, you will see the symbol   

;; @startuml                       
;; :Main Admin: as Admin           
;; (Use the application) as (Use)  
                                
;; User <|-- Admin                 
;; (Start) <|-- (Use)              
                                
;; @enduml                         

;;;;; Using notes

;; You can use the note left of , note right of , note top of , note
;; bottom of keywords to define notes related to a single object.

;; A note can be also define alone with the note keywords, then linked
;; to other objects using the .. symbol.

;; @startuml                                               
;; :Main Admin: as Admin                                   
;; (Use the application) as (Use)                          
                                                        
;; User -> (Start)                                         
;; User --> (Use)                                          
                                                        
;; Admin ---> (Use)                                        
                                                        
;; note right of Admin : This is an example.               
                                                        
;; note right of (Use)                                     
;;   A note can also                                       
;;   be on several lines                                   
;; end note                                                
                                                        
;; note "This note is connected\nto several objects." as N2
;; (Start) .. N2                                           
;; N2 .. (Use)                                             
;; @enduml                                                 

;;;;; Stereotypes

;; You can add stereotypes while defining actors and use cases using "
;; << " and " >> "

;; @startuml                                   
;; User << Human >>                            
;; :Main Database: as MySql << Application >>  
;; (Start) << One Shot >>                      
;; (Use the application) as (Use) << Main >>   
                                            
;; User -> (Start)                             
;; User --> (Use)                              
                                            
;; MySql --> (Use)                             
                                            
;; @enduml                                     

;;;;; Changing arrows direction

;; By default, links between classes have two dashes -- and are
;; verticaly oriented. It is possible to use horizontal link by putting
;; a single dash (or dot) like this:

;; @startuml                
;; :user: --> (Use case 1)  
;; :user: -> (Use case 2)   
;; @enduml                  

;; You can also change directions by reversing the link:

;; @startuml                
;; (Use case 1) <.. :user:  
;; (Use case 2) <- :user:   
;; @enduml                  

;; It is also possible to change arrow direction by adding left, right,
;; up or down keywords inside the arrow:

;; @startuml                      
;; :user: -left-> (dummyLeft)     
;; :user: -right-> (dummyRight)   
;; :user: -up-> (dummyUp)         
;; :user: -down-> (dummyDown)     
;; @enduml                        

;; You can shorten the arrow by using only the first character of the
;; direction (for example, -d- instead of -down-) or the two first
;; characters (-do-).

;; Please note that you should not abuse this functionnality : GraphViz
;; gives usually good results without tweaking.

;;;;; Title the diagram

;; The title keywords is used to put a title.

;; You can use title and end title keywords for a longer title, as in
;; sequence diagrams.

;; @startuml                                    
;; title Simple <b>Usecase</b>\nwith one actor  
                                             
;; "Use the application" as (Use)               
;; User -> (Use)                                
                                             
;; @enduml                                      

;;;;; Splitting diagrams

;; The newpage keywords to split your diagram into several pages or
;; images.

;; @startuml                  
;; :actor1: --> (Usecase1)    
;; newpage                    
;; :actor2: --> (Usecase2)    
;; @enduml                    

;;;;; Left to right direction

;; The general default behaviour when building diagram is top to bottom
;; .

;; @startuml                
;; 'default                 
;; top to bottom direction  
;; user1 --> (Usecase 1)    
;; user2 --> (Usecase 2)    
                         
;; @enduml                  

;; You may change to left to right using the left to right direction
;; command. The result is often better with this direction.

;; @startuml                
                         
;; left to right direction  
;; user1 --> (Usecase 1)    
;; user2 --> (Usecase 2)    
                         
;; @enduml                  

;;;;; Skinparam

;; You can use the skinparam command to change colors and fonts for the
;; drawing.

;; You can use this command :

;;   * In the diagram definition, like any other commands,
;;   * In an included file,
;;   * In a configuration file, provided in the command line or the ANT
;;     task.

;; You can define specific color and fonts for stereotyped actors and
;; usecases.

;; @startuml                                      
                                               
;; skinparam usecase {                            
;;         BackgroundColor DarkSeaGreen           
;;         BorderColor DarkSlateGray              
                                               
;;         BackgroundColor<< Main >> YellowGreen  
;;         BorderColor<< Main >> YellowGreen      
                                               
;;         ArrowColor Olive                       
;;         ActorBorderColor black                 
;;         ActorFontName Courier                  
                                               
;;         ActorBackgroundColor<< Human >> Gold   
;; }                                              
                                               
;; User << Human >>                               
;; :Main Database: as MySql << Application >>     
;; (Start) << One Shot >>                         
;; (Use the application) as (Use) << Main >>      
                                               
;; User -> (Start)                                
;; User --> (Use)                                 
                                               
;; MySql --> (Use)                                
                                               
;; @enduml                                        

;;;;; Complete example

(ert-deftest puml-test-uc-complete-example ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-left-to-right)
     (puml-skinparam (puml-key-val 'packageStyle 'rect))
     (puml-actor 'customer)
     (puml-actor 'clerk)
     )

    (concat
     "left to right direction\n"
     "skinparam packageStyle rect\n"
     "actor customer\n"
     "actor clerk\n"
     "rectangle checkout {\n"
     "  customer -- (checkout)\n"
     "  (checkout) .> (payment) : include\n"
     "  (help) .> (checkout) : extends\n"
     "  (checkout) -- clerk\n"
     "}\n"))))


@startuml                            
left to right direction              
skinparam packageStyle rect          
actor customer                       
actor clerk                          
rectangle checkout {                 
  customer -- (checkout)             
  (checkout) .> (payment) : include  
  (help) .> (checkout) : extends     
  (checkout) -- clerk                
}                                    
@enduml                              


#+header: :file ex2.png
#+begin_src plantuml                            
left to right direction              
skinparam packageStyle rect          
actor customer                       
actor clerk                          
rectangle checkout {                 
  customer -- (checkout)             
  (checkout) .> (payment) : include  
  (help) .> (checkout) : extends     
  (checkout) -- clerk                
}                                    
#+end_src




;;;; Class Diagram
;;;;; Relations between classes

;; @startuml           
;; scale 900 width     
;; Class01 <|-- Class02
;; Class03 *-- Class04 
;; Class05 o-- Class06 
;; Class07 .. Class08  
;; Class09 -- Class10  
;; Class11 <|.. Class12
;; Class13 --> Class14 
;; Class15 ..> Class16 
;; Class17 ..|> Class18
;; Class19 <--* Class20
;; @enduml             

;;;;; Label on relations

;; @startuml                                  
                                           
;; Class01 "1" *-- "many" Class02 : contains  
                                           
;; Class03 o-- Class04 : agregation           
                                           
;; Class05 --> "1" Class06                    
                                           
;; @enduml                                    

;; @startuml                
;; class Car                
                         
;; Driver - Car : drives >  
;; Car *- Wheel : have 4 >  
;; Car -- Person : < owns   
                         
;; @enduml                  

;;;;; Adding methods

;; @startuml                         
;; Object <|-- ArrayList             
                                  
;; Object : equals()                 
;; ArrayList : Object[] elementData  
;; ArrayList : size()                
                                  
;; @enduml                           

;; @startuml                  
;; class Dummy {              
;;   String data              
;;   void methods()           
;; }                          
                           
;; class Flight {             
;;    flightNumber : Integer  
;;    departureTime : Date    
;; }                          
;; @enduml                    

;;;;; Defining visibility

;; +--------------------------------------------------------+
;; |Character|Icon for field|Icon for method|  Visibility   |
;; |---------+--------------+---------------+---------------|
;; |    -    |              |               |private        |
;; |---------+--------------+---------------+---------------|
;; |    #    |              |               |protected      |
;; |---------+--------------+---------------+---------------|
;; |    ~    |              |               |package private|
;; |---------+--------------+---------------+---------------|
;; |    +    |              |               |public         |
;; +--------------------------------------------------------+

;; @startuml          
                   
;; class Dummy {      
;;  -field1           
;;  #field2           
;;  ~method1()        
;;  +method2()        
;; }                  
                   
;; @enduml            

;; You can turn off this feature using the skinparam
;; classAttributeIconSize 0 command :

;; @startuml                           
;; skinparam classAttributeIconSize 0  
;; class Dummy {                       
;;  -field1                            
;;  #field2                            
;;  ~method1()                         
;;  +method2()                         
;; }                                   
                                    
;; @enduml                             

;;;;; Abstract and Static

;; @startuml                    
;; class Dummy {                
;;   {static} String id         
;;   {abstract} void methods()  
;; }                            
;; @enduml                      

;;;;; Advanced class body

;; By default, methods and fields are automatically regrouped by
;; PlantUML. You can use separators to define your own way of ordering
;; fields and methods. The following separators are possible : -- .. ==
;; __.

;; You can also use titles within the separators:

;; @startuml                      
;; class Foo1 {                   
;;   You can use                  
;;   several lines                
;;   ..                           
;;   as you want                  
;;   and group                    
;;   ==                           
;;   things together.             
;;   __                           
;;   You can have as many groups  
;;   as you want                  
;;   --                           
;;   End of class                 
;; }                              
                               
;; class User {                   
;;   .. Simple Getter ..          
;;   + getName()                  
;;   + getAddress()               
;;   .. Some setter ..            
;;   + setName()                  
;;   __ private data __           
;;   int age                      
;;   -- crypted --                
;;   String password              
;; }                              
                               
;; @enduml                        

;;;;; Notes and stereotypes

;; @startuml                                                     
;; class Object << general >>                                    
;; Object <|--- ArrayList                                        
                                                              
;; note top of Object : In java, every class\nextends this one.  
                                                              
;; note "This is a floating note" as N1                          
;; note "This note is connected\nto several objects." as N2      
;; Object .. N2                                                  
;; N2 .. ArrayList                                               
                                                              
;; class Foo                                                     
;; note left: On last defined class                              
                                                              
;; @enduml                                                       

;; @startuml                                      
                                               
;; class Foo                                      
;; note left: On last defined class               
                                               
;; note top of Object                             
;;   In java, <size:18>every</size> <u>class</u>  
;;   <b>extends</b>                               
;;   <i>this</i> one.                             
;; end note                                       
                                               
;; note as N1                                     
;;   This note is <u>also</u>                     
;;   <b><color:royalBlue>on several</color>       
;;   <s>words</s> lines                           
;;   And this is hosted by <img:sourceforge.jpg>  
;; end note                                       
                                               
;; @enduml                                        

;;;;; Note on links

;; @startuml                              
                                       
;; class Dummy                            
;; Dummy --> Foo : A link                 
;; note on link #red: note that is red    
                                       
;; Dummy --> Foo2 : Another link          
;; note right on link #blue               
;;         this is my note on right link  
;;         and in blue                    
;; end note                               
                                       
;; @enduml                                

;;;;; Abstract class and interface

;; @startuml                            
                                     
;; abstract class AbstractList          
;; abstract AbstractCollection          
;; interface List                       
;; interface Collection                 
                                     
;; List <|-- AbstractList               
;; Collection <|-- AbstractCollection   
                                     
;; Collection <|- List                  
;; AbstractCollection <|- AbstractList  
;; AbstractList <|-- ArrayList          
                                     
;; class ArrayList {                    
;;   Object[] elementData               
;;   size()                             
;; }                                    
                                     
;; enum TimeUnit {                      
;;   DAYS                               
;;   HOURS                              
;;   MINUTES                            
;; }                                    
                                     
;; annotation SuppressWarnings          
                                     
;; @enduml                              

;;;;; Using non-letters

;; If you want to use non-letters in the class (or enum...) display,
;; you can either :

;;   * Use the as keyword in the class definition
;;   * Put quotes "" around the class name

;; @startuml                                
;; class "This is my class" as class1       
;; class class2 as "It works this way too"  
                                         
;; class2 *-- "foo/dummy" : use             
;; @enduml                                  

;;;;; Hide attributes, methods...

;; You can parameterize the display of classes using the hide/show
;; command.

;; The basic command is: hide empty members. This command will hide
;; attributes or methods if they are empty.

;; Instead of empty members, you can use:

;;   * empty fields or empty attributes for empty fields,
;;   * empty methods for empty methods,
;;   * fields or attributes which will hide fields, even if they are
;;     described
;;   * methods wich will hide methods, even if they are described
;;   * members wich will hide fields and methods, even if they are
;;     described
;;   * circle for the circled character in front of class name,
;;   * stereotype for the stereotype.

;; You can also provide, just after the hide or show keyword:

;;   * class for all classes,
;;   * interface for all interfaces,
;;   * enum for all enums,
;;   * <<foo1>> for classes which are stereotyped with foo1,
;;   * an existing class name.

;; You can use several show/hide commands to define rules and
;; exceptions.

;; @startuml                        
                                 
;; class Dummy1 {                   
;;   +myMethods()                   
;; }                                
                                 
;; class Dummy2 {                   
;;   +hiddenMethod()                
;; }                                
                                 
;; class Dummy3 <<Serializable>> {  
;;         String name              
;; }                                
                                 
;; hide members                     
;; hide <<Serializable>> circle     
;; show Dummy1 methods              
;; show <<Serializable>> fields     
                                 
;; @enduml                          

;;;;; Hide classes

;; You can also use the show/hide commands to hide classes.

;; This may be usefull if you define a large !included file, and if you
;; want to hide come classes after file inclusion.

;; @startuml          
                   
;; class Foo1         
;; class Foo2         
                   
;; Foo2 *-- Foo1      
                   
;; hide Foo2          
                   
;; @enduml            

;;;;; Use generics

;; You can also use bracket < and > to define generics usage in a
;; class.

;; @startuml                       
                                
;; class Foo<? extends Element> {  
;;   int size()                    
;; }                               
;; Foo *- Element                  
                                
;; @enduml                         

;;;;; Specific Spot

;; Usually, a spotted character (C, I, E or A) is used for classes,
;; interface, enum and abstract classes.

;; But you can define your own spot for a class when you define the
;; stereotype, adding a single character and a color, like in this
;; example:

;; @startuml                                 
                                          
;; class System << (S,#FF7700) Singleton >>  
;; class Date << (D,orchid) >>               
;; @enduml                                   

;;;;; Packages

;; You can define a package using the package keyword, and optionally
;; declare a background color for your package (Using a html color code
;; or name).

;; Note that package definitions can be nested.

;; @startuml                                
                                         
;; package "Classic Collections" #DDDDDD {  
;;   Object <|-- ArrayList                  
;; }                                        
                                         
;; package net.sourceforge.plantuml {       
;;   Object <|-- Demo1                      
;;   Demo1 *- Demo2                         
;; }                                        
                                         
;; @enduml                                  

;;;;; Packages style

;; There are different styles available for packages.

;; You can specify them either by setting a default style with the
;; command : skinparam packageStyle, or by using a stereotype on the
;; package:

;; @startuml                    
;; package foo1 <<Node>> {      
;;   class Class1               
;; }                            
                             
;; package foo2 <<Rect>> {      
;;   class Class2               
;; }                            
                             
;; package foo3 <<Folder>> {    
;;   class Class3               
;; }                            
                             
;; package foo4 <<Frame>> {     
;;   class Class4               
;; }                            
                             
;; package foo5 <<Cloud>> {     
;;   class Class5               
;; }                            
                             
;; package foo6 <<Database>> {  
;;   class Class6               
;; }                            
                             
;; @enduml                      

;; You can also define links between packages, like in the following
;; example:

;; @startuml                     
                              
;; skinparam packageStyle rect   
                              
;; package foo1.foo2 {           
;; }                             
                              
;; package foo1.foo2.foo3 {      
;;   class Object                
;; }                             
                              
;; foo1.foo2 +-- foo1.foo2.foo3  
                              
;; @enduml                       

;;;;; Namespaces

;; In packages, the name of a class is the unique identifier of this
;; class. It means that you cannot have two classes with the very same
;; name in different packages.

;; In that case, you should use namespaces instead of packages.

;; You can refer to classes from other namespaces by fully qualify
;; them. Classes from the default namespace are qualified with a
;; starting dot.

;; Note that you don't have to explicitly create namespace : a fully
;; qualified class is automatically put in the right namespace.

;; @startuml                         
                                  
;; class BaseClass                   
                                  
;; namespace net.dummy #DDDDDD {     
;;     .BaseClass <|-- Person        
;;     Meeting o-- Person            
                                  
;;     .BaseClass <|- Meeting        
;; }                                 
                                  
;; namespace net.foo {               
;;   net.dummy.Person  <|- Person    
;;   .BaseClass <|-- Person          
                                  
;;   net.dummy.Meeting o-- Person    
;; }                                 
                                  
;; BaseClass <|-- net.unused.Person  
                                  
;; @enduml                           

;;;;; Automatic namespace creation

;; You can define another separator (other than the dot) using the
;; command : set namespaceSeparator ???.

;; @startuml                  
                           
;; set namespaceSeparator ::  
;; class X1::X2::foo {        
;;   some info                
;; }                          
                           
;; @enduml                    

;; You can disable automatic package creation using the command set
;; namespaceSeparator none.

;; @startuml                    
                             
;; set namespaceSeparator none  
;; class X1.X2.foo {            
;;   some info                  
;; }                            
                             
;; @enduml                      

;;;;; Lollipop interface

;; You can also define lollipops interface on classes, using the
;; following syntax:

;;   * bar ()- foo
;;   * bar ()-- foo
;;   * foo -() bar

;; @startuml     
;; class foo     
;; bar ()- foo   
;; @enduml       

;;;;; Changing arrows direction

;; By default, links between classes have two dashes -- and are
;; verticaly oriented. It is possible to use horizontal link by putting
;; a single dash (or dot) like this:

;; @startuml          
;; Room o- Studient   
;; Room *-- Chair     
;; @enduml            

;; You can also change directions by reversing the link:

;; @startuml          
;; Studient -o Room   
;; Chair --* Room     
;; @enduml            

;; It is also possible to change arrow direction by adding left, right,
;; up or down keywords inside the arrow:

;; @startuml                 
;; foo -left-> dummyLeft     
;; foo -right-> dummyRight   
;; foo -up-> dummyUp         
;; foo -down-> dummyDown     
;; @enduml                   

;; You can shorten the arrow by using only the first character of the
;; direction (for example, -d- instead of -down-) or the two first
;; characters (-do-).

;; Please note that you should not abuse this functionnality : GraphViz
;; gives usually good results without tweaking.

;;;;; Title the diagram

;; The title keyword is used to put a title.

;; You can use title and end title keywords for a longer title, as in
;; sequence diagrams.

;; @startuml                               
                                        
;; title Simple <b>example</b>\nof title   
;; Object <|-- ArrayList                   
                                        
;; @enduml                                 

;;;;; Legend the diagram

;; The legend and end legend are keywords is used to put a legend.

;; You can optionnaly specify to have left, right or center alignment
;; for the legend.

;; @startuml                             
                                      
;; Object <|- ArrayList                  
                                      
;; legend right                          
;;   <b>Object</b> and <b>ArrayList</b>  
;;   are simple class                    
;; endlegend                             
                                      
;; @enduml                               

;;;;; Association classes

;; You can define association class after that a relation has been
;; defined between two classes, like in this example:

;; @startuml                        
;; class Student {                  
;;   Name                           
;; }                                
;; Student "0..*" - "1..*" Course   
;; (Student, Course) .. Enrollment  
                                 
;; class Enrollment {               
;;   drop()                         
;;   cancel()                       
;; }                                
;; @enduml                          

;; You can define it in another direction:

;; @startuml                        
;; class Student {                  
;;   Name                           
;; }                                
;; Student "0..*" -- "1..*" Course  
;; (Student, Course) . Enrollment   
                                 
;; class Enrollment {               
;;   drop()                         
;;   cancel()                       
;; }                                
;; @enduml                          

;;;;; Skinparam

;; You can use the skinparam command to change colors and fonts for the
;; drawing.

;; You can use this command :

;;   * In the diagram definition, like any other commands,
;;   * In an included file,
;;   * In a configuration file, provided in the command line or the ANT
;;     task.

;; @startuml                                         
                                                  
;; skinparam class {                                 
;;         BackgroundColor PaleGreen                 
;;         ArrowColor SeaGreen                       
;;         BorderColor SpringGreen                   
;; }                                                 
;; skinparam stereotypeCBackgroundColor YellowGreen  
                                                  
;; Class01 "1" *-- "many" Class02 : contains         
                                                  
;; Class03 o-- Class04 : agregation                  
                                                  
;; @enduml                                           

;;;;; Skinned Stereotypes

;; You can define specific color and fonts for stereotyped classes.

;; @startuml                                              
                                                       
;; skinparam class {                                      
;;         BackgroundColor PaleGreen                      
;;         ArrowColor SeaGreen                            
;;         BorderColor SpringGreen                        
;;         BackgroundColor<<Foo>> Wheat                   
;;         BorderColor<<Foo>> Tomato                      
;; }                                                      
;; skinparam stereotypeCBackgroundColor YellowGreen       
;; skinparam stereotypeCBackgroundColor<< Foo >> DimGray  
                                                       
;; Class01 << Foo >>                                      
;; Class01 "1" *-- "many" Class02 : contains              
                                                       
;; Class03<<Foo>> o-- Class04 : agregation                
                                                       
;; @enduml                                                

;;;;; Color gradient

;; It's possible to declare individual color for classes or note using
;; the # notation.
;; You can use either standard color name or RGB code.

;; You can also use color gradient in background, with the following
;; syntax: two colors names separated either by:

;;   * |,
;;   * /,
;;   * \,
;;   * or -

;; depending the direction of the gradient. For example, you could
;; have:

;; @startuml                                            
                                                     
;; skinparam backgroundcolor AntiqueWhite/Gold          
;; skinparam classBackgroundColor Wheat|CornflowerBlue  
                                                     
;; class Foo #red-green                                 
;; note left of Foo #blue\9932CC {                      
;;   this is my                                         
;;   note on this class                                 
;; }                                                    
                                                     
;; package example #GreenYellow/LightGoldenRodYellow {  
;;   class Dummy                                        
;; }                                                    
                                                     
;; @enduml                                              

;;;;; Splitting large files

;; Sometimes, you will get some very large image files.

;; You can use the "page (hpages)x(vpages)" command to split the
;; generated image into several files :

;; hpages is a number that indicated the number of horizontal pages,
;; and vpages is a number that indicated the number of vertical pages.

;; @startuml                       
;; ' Split into 4 pages            
;; page 2x2                        
                                
;; class BaseClass                 
                                
;; namespace net.dummy #DDDDDD {   
;;     .BaseClass <|-- Person      
;;     Meeting o-- Person          
                                
;;     .BaseClass <|- Meeting      
                                
;; }                               
                                
;; namespace net.foo {             
;;   net.dummy.Person  <|- Person  
;;   .BaseClass <|-- Person        
                                
;;   net.dummy.Meeting o-- Person  
;; }                               
                                
;; BaseClass <|-- net.unused.Person
;; @enduml                         

;;;; Activity Diagram

;;;;; Simple Activity


;; (ert-deftest puml-test-simple-activity ()
;;   "ERT test for puml (PlantUML DSL for Emacs Lisp)."
;;   (should
;;    (equal
;;     (puml-pack
;;      (puml-start-activity
;;       (puml-arrow) :nm "First Activity")
;;      (puml-end-activity
;;       (puml-space (puml-arrow) :lead 1)
;;       :nm "First Activity"))
;;     (concat
;;      "(*) --> \"First Activity\"\n"
;;      "\"First Activity\" --> (*)\n"))))

(ert-deftest puml-test-simple-activity ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-start-activity :nm "First Activity")
     (puml-end-activity :nm "First Activity"))
    (concat
     "(*) --> \"First Activity\"\n"
     "\"First Activity\" --> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-start-activity :nm "First Activity")
 (puml-end-activity :nm "First Activity"))

;; #+header: :file puml-test2.png
;; #+header: :exports both
;; #+begin_src plantuml                 
;; (*) --> "First Activity"  
;; "First Activity" --> (*)  
;; #+end_src

;;;;; Label on arrows

(ert-deftest puml-test-label-on-arrows ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-start-activity :nm "First Activity")
     (puml-activity :lbl "You can put also labels"
		    :nm "Second Activity")
     (puml-end-activity))
    (concat
     "(*) --> \"First Activity\"\n"
     "-->[You can put also labels] \"Second Activity\"\n"
     "--> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-start-activity :nm "First Activity")
 (puml-activity :lbl "You can put also labels"
		:nm "Second Activity")
 (puml-end-activity))

;; #+header: :file puml-test4.png
;; #+header: :exports both
;; #+begin_src plantuml                                       
;; (*) --> "First Activity"                        
;; -->[You can put also labels] "Second Activity"  
;; --> (*)                                         
;; #+end_src

;;;;; Changing arrow direction

(ert-deftest puml-test-changing-arrow-direction ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack 
     (puml-start-activity :arr (puml-arrow :dir "up")
			  :nm "First Activity")
     (puml-activity :arr (puml-arrow :dir "right")
		    :nm "Second Activity")
     (puml-activity :nm "Third Activity")
     (puml-end-activity :arr (puml-arrow :dir "left")))
    (concat
     "(*) -up-> \"First Activity\"\n"
     "-right-> \"Second Activity\"\n"
     "--> \"Third Activity\"\n"
     "-left-> (*)\n"))))

;; #+header: :exports code
(puml-pack 
 (puml-start-activity :arr (puml-arrow :dir "up")
		      :nm "First Activity")
 (puml-activity :arr (puml-arrow :dir "right")
		:nm "Second Activity")
 (puml-activity :nm "Third Activity")
 (puml-end-activity :arr (puml-arrow :dir "left")))

;; #+header: :file puml-test6.png
;; #+header: :exports both
;; #+begin_src plantuml                   
                            
;; (*) -up-> "First Activity"  
;; -right-> "Second Activity"  
;; --> "Third Activity"        
;; -left-> (*)                 
                            
;; #+end_src

;;;;; Branches

(ert-deftest puml-test-branches-1 ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-start-activity :nm "Initialisation")
     (puml-newline)
     (puml-if-then-else
      "Some Test"
      (puml-pack
       (puml-space
	(puml-activity :lbl "true"
		       :nm "Some Activity") :lead 2)
       (puml-space
	(puml-activity :nm "Another activity") :lead 2)
       (puml-space
	(puml-end-activity :arr (puml-arrow :dir "right")) :lead 2))
      (puml-pack
       (puml-space
	(puml-activity :arr (puml-arrow :len 1)
		       :lbl "false"
		       :nm "Something else") :lead 2)
       (puml-space
	(puml-end-activity  :lbl "Ending process") :lead 2))))
    (concat
     "(*) --> \"Initialisation\"\n"
     "\n"
     "if \"Some Test\" then\n"
     "  -->[true] \"Some Activity\"\n"
     "  --> \"Another activity\"\n"
     "  -right-> (*)\n"
     "else\n"
     "  ->[false] \"Something else\"\n"
     "  -->[Ending process] (*)\n"
     "endif\n"))))

;; #+header: :exports code
(puml-pack
 (puml-start-activity :nm "Initialisation")
 (puml-newline)
 (puml-if-then-else
  "Some Test"
  (puml-pack
   (puml-space
    (puml-activity :lbl "true"
		   :nm "Some Activity") :lead 2)
   (puml-space
    (puml-activity :nm "Another activity") :lead 2)
   (puml-space
    (puml-end-activity :arr (puml-arrow :dir "right")) :lead 2))
  (puml-pack
   (puml-space
    (puml-activity :arr (puml-arrow :len 1)
		   :lbl "false"
		   :nm "Something else") :lead 2)
   (puml-space
    (puml-end-activity  :lbl "Ending process") :lead 2))))

;; #+header: :file puml-test8.png
;; #+header: :exports both
;; #+begin_src plantuml                     
;; (*) --> "Initialisation"      
                              
;; if "Some Test" then           
;;   -->[true] "Some Activity"   
;;   --> "Another activity"      
;;   -right-> (*)                
;; else                          
;;   ->[false] "Something else"  
;;   -->[Ending process] (*)     
;; endif                         
                              
;; #+end_src

(ert-deftest puml-test-branches-2 ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-start-activity :nm "check input")
     (puml-if-then-else
      "input is verbose"
      (puml-pack
       (puml-activity :lbl "Yes"
		      :nm "turn on verbosity")
       (puml-activity :nm "run command"))
      (puml-activity :nm "run command"))
     (puml-end-activity))
    (concat
     "(*) --> \"check input\"\n"
     "if \"input is verbose\" then\n"
     "-->[Yes] \"turn on verbosity\"\n"
     "--> \"run command\"\n"
     "else\n"
     "--> \"run command\"\n"
     "endif\n"
     "--> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-start-activity :nm "check input")
 (puml-if-then-else
  "input is verbose"
  (puml-pack
   (puml-activity :lbl "Yes"
		  :nm "turn on verbosity")
   (puml-activity :nm "run command"))
  (puml-activity :nm "run command"))
 (puml-end-activity))

;; #+header: :file puml-test10.png
;; #+header: :exports both
;; #+begin_src plantuml                      
;; (*)  --> "check input"         
;; if "input is verbose" then     
;; --> [Yes] "turn on verbosity"  
;; --> "run command"              
;; else                           
;; --> "run command"              
;; endif                          
;; --> (*)                         
;; #+end_src

;;;;; More on Branches
;;;;; Synchronization

(ert-deftest puml-test-synchronization ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-start-activity :crlf nil)
     (puml-space (puml-sync-bar "B1") :lead 1)
     (puml-activity :nm "Parallel Activity 1")
     (puml-activity :crlf nil)
     (puml-space (puml-sync-bar "B2") :lead 1)
     (puml-newline)
     (puml-sync-bar "B1" :crlf nil)
     (puml-space
      (puml-activity  :nm "Parallel Activity 2") :lead 1)
     (puml-activity :crlf nil)
     (puml-space (puml-sync-bar "B2") :lead 1)
     (puml-newline)
     (puml-end-activity))
    (concat
     "(*) --> ===B1===\n"
     "--> \"Parallel Activity 1\"\n"
     "--> ===B2===\n"
     "\n"
     "===B1=== --> \"Parallel Activity 2\"\n"
     "--> ===B2===\n"
     "\n"
     "--> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-start-activity :crlf nil)
 (puml-sync-bar "B1")
 (puml-activity :nm "Parallel Activity 1")
 (puml-activity :crlf nil)
 (puml-sync-bar "B2")
 (puml-newline)
 (puml-sync-bar "B1" :crlf nil)
 (puml-space
  (puml-activity  :nm "Parallel Activity 2") :lead 1)
 (puml-activity :crlf nil)
 (puml-sync-bar "B2")
 (puml-newline)
 (puml-end-activity))

;; #+header: :file puml-test12.png
;; #+header: :exports both
;; #+begin_src plantuml                           
                                    
;; (*) --> ===B1===                    
;; --> "Parallel Activity 1"           
;; --> ===B2===                        
                                    
;; ===B1=== --> "Parallel Activity 2"  
;; --> ===B2===                        
                                    
;; --> (*)                             
                                    
;; #+end_src

;;;;; Long activity description
;;;;; Notes

(ert-deftest puml-test-notes ()
       "ERT test for puml (PlantUML DSL for Emacs Lisp)."
       (should
	(equal
	 (puml-pack
	  (puml-start-activity :nm "Some Activity")
	  (puml-note "This activity has to be defined")
	  (puml-end-activity :nm "Some Activity")
	  (puml-note " This note is on\n several lines"
		     :dir "left"
		     :ml t))
	 (concat
	  "(*) --> \"Some Activity\"\n"
	  "note right : \"This activity has to be defined\"\n"
	  "\"Some Activity\" --> (*)\n"
	  "note left\n"
	  " This note is on\n"
	  " several lines\n"
	  "end note\n"))))

;; #+header: :exports code
	 (puml-pack
	  (puml-start-activity :nm "Some Activity")
	  (puml-note "This activity has to be defined")
	  (puml-end-activity :nm "Some Activity")
	  (puml-note " This note is on\n several lines"
		     :dir "left"
		     :ml t))

;; #+header: :file puml-test14.png
;; #+header: :exports both
;; #+begin_src plantuml                                    
                                             
;; (*) --> "Some Activity"                      
;; note right : This activity has to be defined  
;; "Some Activity" --> (*)                      
;; note left                                    
;;  This note is on                             
;;  several lines                               
;; end note                                     
                                             
;; #+end_src

;;;;; Partition

(ert-deftest puml-test-partition ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     ;; 1st partition
     (puml-partition
      'Conductor
      (puml-pack
       (puml-space
	(puml-start-activity :nm "Climbs on Platform") :lead 2)
       (puml-space (puml-activity :crlf nil) :lead 2)
       (puml-space (puml-sync-bar " S1 ") :lead 1)
       (puml-space (puml-activity :nm 'Bows) :lead 2)))
     (puml-newline)
     ;; 2nd partition
     (puml-partition
      'Audience
      (puml-pack
       (puml-space (puml-sync-bar " S1 " :crlf nil) :lead 2)
       (puml-space (puml-activity :nm 'Applauds) :lead 1))
      :bg "LightSkyBlue")
     (puml-newline)
     ;; 3rd partition
     (puml-partition
      'Conductor
      (puml-pack
       (puml-space
	(puml-activity :crlf nil
		       :pre "Bows ") :lead 2)
       (puml-space (puml-sync-bar " S2 ") :lead 1)
       (puml-space (puml-activity :nm 'WavesArmes) :lead 2)
       (puml-space
	(puml-activity :crlf nil
		       :pre "Applauds ") :lead 2)
       (puml-space (puml-sync-bar " S2 ") :lead 1)))
     (puml-newline)
     ;; 4th partition
     (puml-partition
      'Orchestra
      (puml-pack
       (puml-space
	(puml-activity :pre "WavesArmes "
		       :nm 'Introduction) :lead 2)
       (puml-space (puml-activity :nm "Play music") :lead 2))
      :bg "#CCCCEE"))
    (concat
     "partition Conductor\n"
     "  (*) --> \"Climbs on Platform\"\n"
     "  --> === S1 ===\n"
     "  --> Bows\n\n"
     "end partition\n"
     "\n"
     "partition Audience LightSkyBlue\n"
     "  === S1 === --> Applauds\n\n"
     "end partition\n"
     "\n"
     "partition Conductor\n"
     "  Bows --> === S2 ===\n"
     "  --> WavesArmes\n"
     "  Applauds --> === S2 ===\n\n"
     "end partition\n"
     "\n"
     "partition Orchestra #CCCCEE\n"
     "  WavesArmes --> Introduction\n"
     "  --> \"Play music\"\n\n"
     "end partition\n"))))


;; #+header: :exports code
(puml-pack
 ;; 1st partition
 (puml-partition
  'Conductor
  (puml-pack
   (puml-space
    (puml-start-activity :nm "Climbs on Platform") :lead 2)
   (puml-space (puml-activity :crlf nil) :lead 2)
   (puml-sync-bar " S1 ")
   (puml-space (puml-activity :nm 'Bows) :lead 2)))
 (puml-newline)
 ;; 2nd partition
 (puml-partition
  'Audience
  (puml-pack
   (puml-space (puml-sync-bar " S1 " :crlf nil) :lead 2)
   (puml-space (puml-activity :nm 'Applauds) :lead 1))
  :bg " LightSkyBlue")
 (puml-newline)
 ;; 3rd partition
 (puml-partition
  'Conductor
  (puml-pack
   (puml-space
    (puml-activity :crlf nil
		   :pre 'Bows) :lead 2)
   (puml-sync-bar " S2 ")
   (puml-space (puml-activity :nm 'WavesArmes) :lead 2)
   (puml-space
    (puml-activity :crlf nil
		   :pre 'Applauds) :lead 2)
   (puml-sync-bar " S2 ")))
 (puml-newline)
 ;; 4th partition
 (puml-partition
  'Orchestra
  (puml-pack
   (puml-space
    (puml-activity :pre 'WavesArmes
		   :nm 'Introduction) :lead 2)
   (puml-space (puml-activity :nm "Play music") :lead 2))
  :bg " #CCCCEE"))

;; #+header: :file puml-test16.png
;; #+header: :exports both
;; #+begin_src plantuml                          
                                   
;; partition Conductor {              
;;   (*) --> "Climbs on Platform"     
;;   --> === S1 ===                   
;;   --> Bows                         
;; }                                  
                                   
;; partition Audience LightSkyBlue {  
;;   === S1 === --> Applauds          
;; }                                  
                                   
;; partition Conductor {              
;;   Bows --> === S2 ===              
;;   --> WavesArmes                   
;;   Applauds --> === S2 ===          
;; }                                  
                                   
;; partition Orchestra #CCCCEE {      
;;   WavesArmes --> Introduction      
;;   --> "Play music"                 
;; }                                  
                                   
;; #+end_src

;;;;; Title the diagram

(ert-deftest puml-test-title-the-diagram ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-title "Simple example\nof title" )
     (puml-newline)
     (puml-start-activity :nm "First activity")
     (puml-end-activity))
    (concat
     "title Simple example\nof title\n"
     "\n"
     "(*) --> \"First activity\"\n"
     "--> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-title "Simple example\nof title" )
 (puml-newline)
 (puml-start-activity :nm "First activity")
 (puml-end-activity))

;; #+header: :file puml-test18.png
;; #+header: :exports both
;; #+begin_src plantuml                        
;; title Simple example\nof title   
                                 
;; (*) --> "First activity"         
;; --> (*)                          
;; #+end_src

;;;;; Skinparam

(ert-deftest puml-test-skinparam ()
  "ERT test for puml (PlantUML DSL for Emacs Lisp)."
  (should
   (equal
    (puml-pack
     (puml-skinparam (puml-key-val "backgroundColor" "#AAFFFF"))
     (puml-skinparam
      (puml-pack
       (puml-space
	(puml-key-value  "StartColor" "red") :lead 2)
       (puml-space
	(puml-key-value  "BarColor" "SaddleBrown") :lead 2)
       (puml-space
	(puml-key-value  "EndColor" "Silver") :lead 2)
       (puml-space
	(puml-key-value  "BackgroundColor" "Peru") :lead 2)
       (puml-space
	(puml-key-value  "BackgroundColor" "Olive") :lead 2)
       (puml-space
	(puml-key-value  "BorderColor" "Peru") :lead 2)
       (puml-space
	(puml-key-value  "FontName" "Impact") :lead 2))
      :elem 'activity)
     (puml-newline)
     (puml-start-activity :nm "Climbs on Platform"
			  :suf (puml-space
				(puml-stereotype
				 " Begin " :crlf nil)
				:lead 1))
     (puml-activity :crlf nil)
     (puml-space (puml-sync-bar " S1 ") :lead 1)
     (puml-activity :nm "Bows")
     (puml-activity :crlf nil)
     (puml-space (puml-sync-bar " S2 ") :lead 1)
     (puml-activity :nm "WavesArmes")
     (puml-end-activity))
    ;; (puml-pack
    ;;  (puml-skinparam "backgroundColor" "#AAFFFF")
    ;;  (puml-multiline-skinparam
    ;;   "activity"
    ;;   (puml-pack
    ;;    (puml-space
    ;; 	(puml-key-value  "StartColor" "red") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "BarColor" "SaddleBrown") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "EndColor" "Silver") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "BackgroundColor" "Peru") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "BackgroundColor" "Olive") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "BorderColor" "Peru") :lead 2)
    ;;    (puml-space
    ;; 	(puml-key-value  "FontName" "Impact") :lead 2)))
    ;;  (puml-newline)
    ;;  (puml-start-activity :nm "Climbs on Platform"
    ;; 			  :suf (puml-stereotype " Begin "))
    ;;  (puml-activity :crlf nil)
    ;;  (puml-sync-bar " S1 ")
    ;;  (puml-activity :nm "Bows")
    ;;  (puml-activity :crlf nil)
    ;;  (puml-sync-bar " S2 ")
    ;;  (puml-activity :nm "WavesArmes")
    ;;  (puml-end-activity))
    (concat
     "skinparam backgroundColor #AAFFFF\n"
     "skinparam activity {\n"
     "  StartColor red\n"
     "  BarColor SaddleBrown\n"
     "  EndColor Silver\n"
     "  BackgroundColor Peru\n"
     "  BackgroundColor Olive\n"
     "  BorderColor Peru\n"
     "  FontName Impact\n"
     "}\n"
     "\n"
     "(*) --> \"Climbs on Platform\" << Begin >>\n"
     "--> === S1 ===\n"
     "--> \"Bows\"\n"
     "--> === S2 ===\n"
     "--> \"WavesArmes\"\n"
     "--> (*)\n"))))

;; #+header: :exports code
(puml-pack
 (puml-skinparam (puml-key-val "backgroundColor" "#AAFFFF"))
 (puml-skinparam
  (puml-pack
   (puml-space
    (puml-key-value  "StartColor" "red") :lead 2)
   (puml-space
    (puml-key-value  "BarColor" "SaddleBrown") :lead 2)
   (puml-space
    (puml-key-value  "EndColor" "Silver") :lead 2)
   (puml-space
    (puml-key-value  "BackgroundColor" "Peru") :lead 2)
   (puml-space
    (puml-key-value  "BackgroundColor" "Olive") :lead 2)
   (puml-space
    (puml-key-value  "BorderColor" "Peru") :lead 2)
   (puml-space
    (puml-key-value  "FontName" "Impact") :lead 2))
  :elem 'activity)
 (puml-newline)
 (puml-start-activity :nm "Climbs on Platform"
		      :suf (puml-space
			    (puml-stereotype
			     " Begin " :crlf nil)
			    :lead 1))
 (puml-activity :crlf nil)
 (puml-space (puml-sync-bar " S1 ") :lead 1)
 (puml-activity :nm "Bows")
 (puml-activity :crlf nil)
 (puml-space (puml-sync-bar " S2 ") :lead 1)
 (puml-activity :nm "WavesArmes")
 (puml-end-activity))

;; #+header: :file puml-test20.png
;; #+header: :exports both
;; #+begin_src plantuml                                 
                                          
;; skinparam backgroundColor #AAFFFF         
;; skinparam activity {                      
;;   StartColor red                          
;;   BarColor SaddleBrown                    
;;   EndColor Silver                         
;;   BackgroundColor Peru                    
;;   BackgroundColor<< Begin >> Olive        
;;   BorderColor Peru                        
;;   FontName Impact                         
;; }                                         
                                          
;; (*) --> "Climbs on Platform" << Begin >>  
;; --> === S1 ===                            
;; --> Bows                                  
;; --> === S2 ===                            
;; --> WavesArmes                            
;; --> (*)                                   
                                          
;; #+end_src

;;;;; Octagon
;;;;; Complete example
;;;; Component Diagram
;;;;; Components
;;;;; Interfaces
;;;;; Basic example
;;;;; Using notes
;;;;; Grouping components
;;;;; Changing arrows direction
;;;;; Title the diagram
;;;;; Skinparam

;;;; State Diagram
;;;;; Simple State
;;;;; Composite state
;;;;; Long name 
;;;;; Concurrent state
;;;;; Arrow direction
;;;;; Note
;;;;; More in notes

;;;; Object Diagram
;;;;; Definition of objects
;;;;; Relation between objects
;;;;; Adding fields

;;;; Common commands
;;;;; Footer and header
;;;;; Zoom
;;;;; Rotation

;;;; Changing fonts and colors
;;;;; Usage

;;; Functions

(defun puml-test-wrap-example-in-src-block (content int &optional insert-p)
 "Return or insert CONTENT of start-/enduml block as Org src-block.
Output file is numbered with INT."
 (let ((wrapped-cont
	(format
	 (concat
	  "\n#+header: :file ex%d.png\n"
	  "#+begin_src plantuml%s#+end_src\n")
	 int content)))
       (if insert-p (insert wrapped-cont) wrapped-cont)))

;;; Commands

(defun puml-test-uml-to-src (&optional beg end append-p)
  "Replace (plant)uml-blocks between BEG/END with Org src_blocks.
If APPEND-P is non-nil, append source-block to uml-block instead of replacing it."
  (interactive "r")
  (let ((from (or beg (point-min)))
	(till (or end (point-max)))
	(counter 0)
	(appendp (or append-p current-prefix-arg)))
    (goto-char from)
    (while (ignore-errors
	     (re-search-forward
	      (concat
	       "^\\(?:[[:space:]]*@startuml\\)"
	       "\\([^ ]+?\\)\\(?:@enduml[[:space:]]*\\)$")
	      till 'NORERROR))
      (let ((match (match-string 1))
	    (match-beg (match-beginning 0))
	    (match-end (match-end 0)))
	(setq counter (1+ counter))
	(if (not appendp)
	    (delete-region match-beg match-end)
	  (goto-char match-end)
	  (newline 2))
	(puml-test-wrap-example-in-src-block
	 match counter 'INSERT-P)))))
  
;;; Run Hooks and Provide

(provide 'puml-test)

;;; puml-test.el ends here
