package com.mfe.baruch.capstone;

import java.util.ArrayList;
import java.util.List;

public class TothRunner {

	/**
	 * @param args
	 */
	@SuppressWarnings("unchecked")
	public static void main(String[] args) {
	    long start = System.currentTimeMillis();
		Parameters p = new Parameters();
		
		TothSimulator simulator = new TothSimulator(p);
		List<Book> books = simulator.runSimulations();
		System.out.println("Time to run " + p.getNumSims() + ": " + (System.currentTimeMillis() - start));
		
		List<Integer> aveBookShape = new ArrayList<Integer>(2*p.getBand()+1);
		
		for(Book book:books) {
			Utils.aggregateLists(aveBookShape, book.dynamicBookShape(p.getBand()));
			
		}
		Utils.write(aveBookShape, "C:/aya/Documents/MFE/Baruch/Capstone/AveBookShape.csv");
		System.out.println("Done");
	}

}
