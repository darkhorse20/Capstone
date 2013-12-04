package com.mfe.baruch.capstone;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;



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
    
    public static void write(List book, String path) {
        File f = new File(path);
        
        try {
            f.createNewFile();
      
            FileWriter fw = new FileWriter(f);    
            for(int i=0; i < book.size(); i++) {
                fw.write(book.get(i)+ "\n");
                
            }
            fw.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
            
        }
        
    }
    public static void aggregateLists(List<Integer> to, List<Integer> from) {
    	for(int i=0; i<from.size(); i++) {
    		if(to.size() > i) {
    			to.set(i, (from.get(i) + to.get(i))/2);	
    		} else {
    			to.add(i, from.get(i));
    		}
    		
    	}
    }
    
    public static void aggregateListsDouble(List<Double> to, List<Double> from) {
    	for(int i=0; i<from.size(); i++) {
    		if(to.size() > i) {
    			to.set(i, (from.get(i) + to.get(i))/2);	
    		} else {
    			to.add(i, from.get(i));
    		}
    		
    	}
    }
}
