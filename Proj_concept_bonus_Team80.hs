type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

checkRepeated Null _=
 False  
checkRepeated (S (x,y) mines s t) (x1,y1)=
 if(x==x1&&y==y1) then True
 else checkRepeated t (x1,y1)

up :: MyState -> MyState

up Null= 
 Null
up (S (x,y) mines s t)=
 if x ==0 || checkRepeated (S (x,y) mines s t) (x-1,y) then Null
 else 
  (S (x-1,y) mines "up" (S (x,y) mines s t))
  

 
down:: MyState -> Int -> MyState

down Null _=
 Null
down (S (x,y) mines s t) maxX=
 if x+1>maxX || checkRepeated (S (x,y) mines s t) (x+1,y) then Null
 else 
  (S (x+1,y) mines "down" (S (x,y) mines s t))
  
  
left:: MyState -> MyState

left Null=
 Null
left (S (x,y) mines s t)=
 if y==0 || checkRepeated (S (x,y) mines s t) (x,y-1)then Null
 else 
  (S (x,y-1) mines "left" (S (x,y) mines s t))


right::  MyState -> Int -> MyState

right Null _=
 Null
right (S (x,y) mines s t) maxY=
 if y+1>maxY || checkRepeated (S (x,y) mines s t) (x,y+1) then Null
 else 
  (S (x,y+1) mines "right" (S (x,y) mines s t))

 
collect:: MyState -> MyState

collect Null=
 Null

collect (S robotPosition mines s t)
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
 



nextMyStates::MyState->Int->Int->[MyState]

nextMyStates Null _ _=
 []
nextMyStates x maxX maxY=
  checkCollect x ++ checkUp x ++ (checkDown x maxX)++ (checkRight x maxY)++ checkLeft x
  

checkUp x=
  if (up x)/=Null then (up x) :  []
  else
  []

checkDown x maxX =
  if (down x maxX)/=Null then (down x maxX) : []
  else
  []

checkLeft x=
  if (left x)/=Null then (left x) : [] 
  else
  []

checkRight x maxY=
  if (right x maxY)/=Null then (right x maxY) : []
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
 
 
search::[MyState]->Int->Int->MyState


search ( (S position mines s t1):t2 ) maxX maxY=
 if isGoal (S position mines s t1) then (S position mines s t1)
 else
   search ( t2 ++ (nextMyStates(S position mines s t1) maxX maxY) ) maxX  maxY
  

constructSolution:: MyState ->[String]

constructSolution (S (x,y) mines "" t)=
 []
constructSolution (S (x,y) mines s t)=
 constructSolution t ++[s]

  

solve :: Cell->[Cell]->[String]

solve robotPosition mines=
   constructSolution (search ( nextMyStates (S robotPosition mines "" Null)  (getMaxX robotPosition mines) 
                                                                             (getMaxY robotPosition mines)     
   ) (getMaxX robotPosition mines) (getMaxY robotPosition mines) )





getMaxX :: Cell ->[Cell]->Int
getMaxX (x,y) []=x
getMaxX (x1,y1) ((x2,y2):t)=
 if x1<x2 then getMaxX (x2,y1) t 
 else getMaxX (x1,y1) t
 
getMaxY :: Cell ->[Cell]->Int
getMaxY (x,y) []=y
getMaxY (x1,y1) ((x2,y2):t)=
 if y1<y2 then getMaxY (x1,y2) t 
 else getMaxY (x1,y1) t