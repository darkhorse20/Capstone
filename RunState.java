package com.mfe.baruch.capstone;

import org.apache.commons.math3.distribution.BinomialDistribution;
import org.apache.commons.math3.distribution.PoissonDistribution;
import org.apache.commons.math3.distribution.UniformRealDistribution;

public class RunState {
	private int currL;
	private int currBS;
	private Book book;
	private PoissonDistribution mktPois = new PoissonDistribution(0.1);
	private PoissonDistribution cxlPois = new PoissonDistribution(0.0001);
	private PoissonDistribution limPois = new PoissonDistribution(0.5);
	private UniformRealDistribution fracUnif;
	private UniformRealDistribution bsUnif = new UniformRealDistribution();
	private BinomialDistribution cxlBS;
	private BinomialDistribution mktBS;
	private Parameters runParams;
	
	public RunState(Book book, Parameters params) {
		this.currL = 0;
		this.currBS = 2;
		this.book = book;
		this.runParams = params;
		cxlBS = new BinomialDistribution(1, 0.5);
		mktBS = new BinomialDistribution(1, 0.5);
		fracUnif = new UniformRealDistribution(params.getZetaG(),1.0);
	}


    public int getCurrL() {
        return currL;
    }


    public int getCurrBS() {
        return currBS;
    }


    public Book getBook() {
        return book;
    }


    public PoissonDistribution getMktPois() {
        return mktPois;
    }


    public PoissonDistribution getCxlPois() {
        return cxlPois;
    }


    public PoissonDistribution getLimPois() {
        return limPois;
    }


    public UniformRealDistribution getFracUnif() {
        return fracUnif;
    }


    public UniformRealDistribution getBsUnif() {
        return bsUnif;
    }


    public BinomialDistribution getCxlBS() {
        return cxlBS;
    }


    public void setCxlBS(BinomialDistribution cxlBS) {
        this.cxlBS = cxlBS;
    }


    public BinomialDistribution getMktBS() {
        return mktBS;
    }


    public void setMktBS(BinomialDistribution mktBS) {
        this.mktBS = mktBS;
    }


    public Parameters getRunParams() {
        return runParams;
    }


    public void setRunParams(Parameters runParams) {
        this.runParams = runParams;
    }


    public void setCurrL(int currL) {
        this.currL = currL;
    }


    public void setCurrBS(int currBS) {
        this.currBS = currBS;
    }


    public void setBook(Book book) {
        this.book = book;
    }


    public void setMktPois(PoissonDistribution mktPois) {
        this.mktPois = mktPois;
    }


    public void setCxlPois(PoissonDistribution cxlPois) {
        this.cxlPois = cxlPois;
    }


    public void setLimPois(PoissonDistribution limPois) {
        this.limPois = limPois;
    }


    public void setFracUnif(UniformRealDistribution fracUnif) {
        this.fracUnif = fracUnif;
    }


    public void setBsUnif(UniformRealDistribution bsUnif) {
        this.bsUnif = bsUnif;
    }
	
	
}
