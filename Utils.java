package com.mfe.baruch.capstone;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;



public class Utils {
    public static void writeBook(Book book, String path) {
        File f = new File(path);
        
        try {
            f.createNewFile();
      
            FileWriter fw = new FileWriter(f);    
            for(int i=0; i < book.getnL()+1; i++) {
                fw.write(book.getPrices().get(i) + "," + book.getBuys().get(i) + "," + book.getSells().get(i) + "\n");
                
            }
            fw.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
            
        }
        
    }
}
