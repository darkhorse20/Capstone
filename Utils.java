package com.mfe.baruch.capstone;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation;



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
    
    public static void writeBooks(List<List<Integer>> books, String path) {
        File f = new File(path);
        
        try {
            f.createNewFile();
      
            FileWriter fw = new FileWriter(f); 
            for(int i=0; i < books.size(); i++) {
                   for(int j=0; j< books.get(i).size(); j++) {
                       fw.write(books.get(i).get(j)+ ",");       
                   }
                   fw.write("\n");
                
            }
            fw.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
            
        }
        
    }
    
    public static void aggregateLists(List<Integer> to, List<Integer> from) {
        if(from == null) {
            return;
        }
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
    
    public static void addBook(List<List<Integer>> to, List<Integer> from) {
        to.add(from);
    }
    
    public static List<Double> calcStdDev(List<List<Integer>> allBooks) {
        
        int bkSize = allBooks.get(0).size();
        double[] dvals = new double[bkSize+1];
        double sqrtn = Math.sqrt(bkSize+1);
        
        //List<Double> vals = new ArrayList<Double>(bkSize);
        
        List<Double> sds = new ArrayList<Double>(bkSize);
        
        org.apache.commons.math.stat.descriptive.moment.StandardDeviation sd = new org.apache.commons.math.stat.descriptive.moment.StandardDeviation();
        for(int j=0; j < allBooks.get(0).size(); j++) {
            for(int i=0; i<allBooks.size(); i++) {
                   dvals[j] = allBooks.get(i).get(j);
            }
            sds.add(sd.evaluate(dvals)/sqrtn);
        }
        return sds;
    }
}
