package com.mfe.baruch.capstone;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class ParallelTothRunner {

    /**
     * @param args
     */
    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        if(args.length < 4) {
            System.out.println("Incorrect nr of params, run as: <jdk path>/java nrofsims nrofevents baspath");
            System.exit(1);
            
        }
        int nrSims = Integer.parseInt(args[0]);
        int nrEvents = Integer.parseInt(args[1]);
        int nrThrds = Integer.parseInt(args[2]);
        String basePath = args[3];
        
        long start = System.currentTimeMillis();
        Parameters p = new Parameters(nrSims, nrEvents, nrThrds, basePath);
        List<Book> books = new ArrayList<Book>();
        List<Double> aveMidPrxs = new ArrayList<Double>(p.getNumEvents());
        ExecutorService executor =Executors.newFixedThreadPool(p.getThreads());
        List<Future<RunState>> futureList = new ArrayList<Future<RunState>>(p.getNumEvents());
        
        for (int i=0; i < p.getNumSims(); i++) {
            futureList.add(executor.submit(new CallableTothSimulator(p)));
        }
        
        for(int i=0;i < futureList.size(); i++) {
            try {
                //System.out.println("Waiting for future : " + i);
                RunState state = futureList.get(i).get();
                books.add(state.getBook());
                Utils.aggregateListsDouble(aveMidPrxs, state.getMidPrxs());
                System.out.println("Waiting for future : " + i + " done");
                
            } catch (InterruptedException e) {
                throw new RuntimeException( e );
                
            } catch (ExecutionException e) {
                throw new RuntimeException( e );

            }
        }
        System.out.println("Time to run " + p.getNumSims() + ": " + (System.currentTimeMillis() - start));
        executor.shutdown();
        
        Utils.write(aveMidPrxs, p.getOutputFilePath() + "AveMidPrxs.csv");
        
        List<Integer> aveBookShape = new ArrayList<Integer>(2*p.getBand()+1);
        List<List<Integer>> allbooks = new ArrayList<List<Integer>>(p.getNumSims());
        
        int i =0;
        for(Book book:books) {
            List<Integer> bookShape = book.dynamicBookShape(p.getBand());
            Utils.aggregateLists(aveBookShape, bookShape);
            allbooks.add(bookShape);
            i++;
        }
        
        Utils.write(aveBookShape, p.getOutputFilePath() + "AveBookShape.csv");
        Utils.write(Utils.calcStdDev(allbooks), p.getOutputFilePath() + "BookShapes.csv");
        Utils.writeBooks(allbooks, p.getOutputFilePath() + "All.csv");
        
        
        System.out.println("Done");
    }

}
