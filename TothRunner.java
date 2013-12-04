package com.mfe.baruch.capstone;

public class TothRunner {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
	    long start = System.currentTimeMillis();
		Parameters p = new Parameters();
		
		TothSimulator simulator = new TothSimulator(p);
		simulator.runSimulations();
		System.out.println("Time to run " + p.getNumSims() + ": " + (System.currentTimeMillis() - start));
		
		System.out.println("Done");
	}

}
