
timeout(10); // timeout to 10 ms 

while(1) 
{ 
int ch = getch(); // get character blocking 10 ms at max 
if(ch != ERR) // key pressed 
{ 
// do something with the key such as update screen 
// or launch another process 
} 
else 
{ 
// no keypress in last 10 ms so poll progress info 
get_progress_info(); 
} 
} 

