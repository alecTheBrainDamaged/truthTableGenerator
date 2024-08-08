# truthTableGenerator
A truth table generator for propositional logic formulas , and prop logic arguments to check validity


this is a huge work in progress. 
many additions will come and I welcome anybody to help as long as I can understand what they are doing. 

Here are some examples of how the back end will parse these strings, and the front end will render a truth table. 
These Strings are both considered arguments , where the % symbol is a therefore symbol . It should really look like this -> ∴. 

firstString = " P <-> Q % P -> Q"
secondString = "P <-> Q
% P -> Q  
and it will generate a truth table for them.
whether its an argument ,
or just a set of proposotions.
the % symbol is a placeholder for the therefore symbol. 
<img src="blob:chrome-untrusted://media-app/bec573b7-1fe0-4b0e-aff6-076af581cea3" alt="Screenshot 2024-08-08 10.56.53 AM.png"/>![image](https://github.com/user-attachments/assets/36dc8bfc-b120-4faf-a676-9e56d9f6a489)
I lied, some Strings will result in a parsing error. like the second string above. 
<img src="blob:chrome-untrusted://media-app/ed728572-3994-4add-a9ff-dd3f71900e83" alt="Screenshot 2024-08-08 10.56.53 AM.png"/>![image](https://github.com/user-attachments/assets/0a454849-7ee2-41dc-90b3-b29ef222299d)
