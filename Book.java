package com.mfe.baruch.capstone;

import java.util.ArrayList;
import java.util.List;

public class Book {
	private List<Double> prices;
	private List<Integer> sells;
	private List<Integer> buys;
	private int nL;
	private Parameters params;
	
	@SuppressWarnings("unchecked")
	public Book(Parameters params) {
		this.nL = (int) Math.round(params.getLL()/params.getScale());
		this.params = params;
		
		prices = new ArrayList(nL+1);
		setBookPrices(prices, params);

		buys = new ArrayList(nL+1);
		
		int mid = nL/2;
		for(int i=0; i<mid-params.getL(); i++) {
			buys.add(i, 5000);
		}

		for(int i=0; i<params.getL(); i++) {
			buys.add(mid -params.getL() + i, 5000 - (i+1)*params.getSizeIncr());
		}

		for(int i=mid; i<nL+1; i++) {
			buys.add(i, 0);
		}

		sells = new ArrayList(nL+1);
		
		for(int i=0; i<mid; i++) {
			sells.add(i, 0);
		}

		for(int i=0; i<params.getL(); i++) {
			sells.add(mid + i, (i+1)*params.getSizeIncr());
		}

		for(int i=mid+params.getL(); i<nL+1; i++) {
			sells.add(i, 5000);
		}
		//System.out.println("Book set up");

	}
	
	private void setBookPrices(List attrib, Parameters params) {
		
		for(int i=0; i<this.nL +1; i++) {
			attrib.add(i, i*params.getScale());
		}

	}
	
	public List dynamicBookShape(int band) {
		List bookShape = new ArrayList<Integer>(2*band +1);
		int midP = midPosn();
		int buyStart = midP - band;
		int sellStart = midP;
		for(int i=0; i < 2*band; i++) {
			if(i<band) bookShape.add(i,this.buys.get(buyStart+i));
			else bookShape.add(i, this.sells.get(buyStart + i));
		}
		return bookShape;
	}
	
	public int askPosn() {
		for(int i=0; i < nL+1; i++) {
			if( sells.get(i) > 0) {
				return i;
			}
		}
		return 0;
	}

	public int bidPosn() {
		for(int i=nL-1; i >= 0; i--) {
			if( buys.get(i) > 0) {
				return i;
			}
		}
		return 0;
	}
	
	public int midPosn() {
		return (askPosn() + bidPosn())/2;
	}
	
	public double bestOffer() {
		return prices.get(askPosn());
	}
	
	public double bestBid() {
		return prices.get(bidPosn());
	}
	
	public double mid() {
		return (bestOffer() + bestBid())/2;
	}
	
	public int getNrBuys() {
		int nb = 0;
		int start = askPosn() - params.getL();
		for(int i=0 ; i < params.getL(); i++) {
			nb += buys.get(start + i);
		}
		return nb;
	} 
	
	public int getNrSells() {
		int ns = 0;
		int start = bidPosn();
		for(int i=0 ; i < params.getL(); i++) {
			ns += sells.get(start + i);
		}
		return ns;
	}

    public List<Double> getPrices() {
        return prices;
    }

    public void setPrices(List<Double> prices) {
        this.prices = prices;
    }

    public List<Integer> getSells() {
        return sells;
    }

    public void setSells(List<Integer> sells) {
        this.sells = sells;
    }

    public List<Integer> getBuys() {
        return buys;
    }

    public void setBuys(List<Integer> buys) {
        this.buys = buys;
    }

    public int getnL() {
        return nL;
    }

    public void setnL(int nL) {
        this.nL = nL;
    }

    public Parameters getParams() {
        return params;
    }

    public void setParams(Parameters params) {
        this.params = params;
    }
	
	
	
}
