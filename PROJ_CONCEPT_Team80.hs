type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


up :: MyState -> MyState

up Null= 
 Null
up (S (x,y) mines s t)=
 if x ==0 then Null
 else 
  (S (x-1,y) mines "up" (S (x,y) mines s t))
  
  
down:: MyState -> MyState

down Null=
 Null
down (S (x,y) mines s t)=
 if x ==3 then Null
 else 
  (S (x+1,y) mines "down" (S (x,y) mines s t))
  
  
left:: MyState -> MyState

left Null=
 Null
left (S (x,y) mines s t)=
 if y==0 then Null
 else 
  (S (x,y-1) mines "left" (S (x,y) mines s t))


right:: MyState -> MyState

right Null=
 Null
right (S (x,y) mines s t)=
 if y==3 then Null
 else 
  (S (x,y+1) mines "right" (S (x,y) mines s t))

 
collect:: MyState -> MyState

collect Null=
 Null

collect (S robotPosition mines s t)
 --0| noCollect (S robotPosition mines s t) = Null

 |(element robotPosition mines) =  
 S (robotPosition) (delete robotPosition mines) "collect" (S robotPosition mines s t)

 |otherwise=
 Null


--noCollect Null=
-- True
--noCollect (S robotPosition mines s t)=
-- if(element robotPosition mines) then  
-- False
-- else
--  noCollect t
 


--what is the type of the delete function
delete _ []=
 []
delete (x,y) ((x1,y1):t)=
 if (x==x1&&y==y1) then t
 else
 (x1,y1): delete (x,y) t

--what is the type of the element function
element _ []=
 False
element (x,y) ((x1,y1):t)=
 if (x==x1&&y==y1) then True
 else 
 element (x,y) t
 



nextMyStates::MyState->[MyState]

nextMyStates Null=
 []
nextMyStates x=
  checkCollect x ++ checkUp x ++ checkDown x ++ checkRight x ++ checkLeft x
  

checkUp x=
  if (up x)/=Null then (up x) :  []
  else
  []

checkDown x=
  if (down x)/=Null then (down x) : []
  else
  []

checkLeft x=
  if (left x)/=Null then (left x) : [] 
  else
  []

checkRight x=
  if (right x)/=Null then (right x) : []
  else
  []
checkCollect x=
 if (collect x)/=Null then (collect x): []
 else 
 []
  
isGoal::MyState->Bool

--isGoal Null=
-- False
isGoal (S (x,y) mines s t)=
 if mines==[] then True
 else
 False 
-- isGoal t
 
 
search::[MyState]->MyState


search ( (S position mines s t1):t2 )=
 if isGoal (S position mines s t1) then (S position mines s t1)
 else
   search ( t2 ++ nextMyStates(S position mines s t1) )
  

constructSolution:: MyState ->[String]

constructSolution (S (x,y) mines "" t)=
 []
constructSolution (S (x,y) mines s t)=
 constructSolution t ++[s]

  

solve :: Cell->[Cell]->[String]

solve robotPosition mines=
   constructSolution (search (nextMyStates(S robotPosition mines "" Null)) )






 
 
 
 
 
 
 
 
 

  

 







 
 
 
 
 
 
 
 
 
 
 
 
 
