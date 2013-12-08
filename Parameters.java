package com.mfe.baruch.capstone;

public class Parameters {
    
    private int threads = 7;
    private int band = 150;
    private int numSims = 20000;
    private int numEvents = 10000;
    private int LL = 1;
    private double scale = 0.001;
    private int L = 100;
    private int sizeIncr = 50;
    private double zetaG = 0.65;
    private double nu = 0.0001;
    private double alpha = 1.8;
    //private String outputFilePath = "C:/aya/Documents/MFE/Baruch/Capstone/TothBook";
    private String outputFilePath = "C:\\MSDE\\anuroopy\\Capstone/TothBook";
    
    
    public double getNu() {
        return nu;
    }
    public void setNu(double nu) {
        this.nu = nu;
    }
    public int getNumSims() {
        return numSims;
    }
    public void setNumSims(int numSims) {
        this.numSims = numSims;
    }
    public int getNumEvents() {
        return numEvents;
    }
    public void setNumEvents(int numEvents) {
        this.numEvents = numEvents;
    }
    public int getLL() {
        return LL;
    }
    public void setLL(int lL) {
        LL = lL;
    }
    public double getScale() {
        return scale;
    }
    public void setScale(double scale) {
        this.scale = scale;
    }
    public int getL() {
        return L;
    }
    public void setL(int l) {
        L = l;
    }
    public int getSizeIncr() {
        return sizeIncr;
    }
    public void setSizeIncr(int sizeIncr) {
        this.sizeIncr = sizeIncr;
    }
    public double getZetaG() {
        return zetaG;
    }
    public void setZetaG(double zetaG) {
        this.zetaG = zetaG;
    }
    public String getOutputFilePath() {
        return outputFilePath;
    }
    public void setOutputFilePath(String outputFilePath) {
        this.outputFilePath = outputFilePath;
    }
    public int getBand() {
        return band;
    }
    public void setBand(int band) {
        this.band = band;
    }
    public double getAlpha() {
        return alpha;
    }
    public void setAlpha(double alpha) {
        this.alpha = alpha;
    }
    public int getThreads() {
        return threads;
    }
    public void setThreads(int threads) {
        this.threads = threads;
    }
    
    
}
