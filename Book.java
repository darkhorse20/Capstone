package com.mfe.baruch.capstone;

import java.util.ArrayList;
import java.util.List;

public class Book {
    private List<Double> prices;
    private List<Integer> sells;
    private List<Integer> buys;
    private int nL;
    private Parameters params;
    
    //State variables
    private int askPosn=0;
    private int bidPosn =0;
    
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
        
        askPosn = askPosn();
        bidPosn = bidPosn();
        
//        System.out.println("Book set up, current Bid: " + bidPosn + ", current Ask: " + askPosn + "\n");

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
        //int sellStart = midP;
        //If the mid prices is too high or too low, reject.

        if(buyStart < 0 || (buyStart +band) > nL) {
            System.out.println("Edge case, mid posn is: " + midP);
            return null;
            
        }
        
        for(int i=0; i < 2*band; i++) {
            if(i<=band) {
                bookShape.add(i,this.buys.get(buyStart+i));
            } else {
//                if((buyStart + i) > this.nL) {
//                    bookShape.add(i,5000);
//                } else {
                    bookShape.add(i, this.sells.get(buyStart + i));    
//                }
            }
            
        }
        return bookShape;
    }
    
    public int askPosn() {
        
        if(this.askPosn <=0) {
            for(int i=0; i < nL+1; i++) {
                if( sells.get(i) > 0) {
                    return i;
                }
            }
            
        } else {
            return askPosn;
        }
        return 0;
   
/*        if(sells.get(nL/2) == 0) {
            for(int i=nL/2+1; i<nL; i++) {
                if( sells.get(i) > 0) {
                    return i;
                }
            }
        } else {
            for(int i=nL/2-1; i>0; i--) {
                if( sells.get(i) == 0) {
                    return (i+1);
                }
            }
        }
        return 0;*/
     }

    public int bidPosn() {
        if(this.bidPosn <= 0) {
            for(int i=nL-1; i >= 0; i--) {
                if( buys.get(i) > 0) {
                    return i;
                }
            }
            
        } else {
               return bidPosn;
        }
        return 0;
        
/*        if(buys.get(nL/2) == 0) {
            for(int i=nL/2-1; i>0; i--) {
                if( buys.get(i) > 0) {
                    return i;
                }
            }
        } else {
            for(int i=nL/2+1; i<nL; i++) {
                if( buys.get(i) == 0) {
                    return (i-1);
                }
            }
        }
        return 0;*/
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
        int start = askPosn() - params.getL()-1;
        int end = params.getL();
       
        for(int i=0 ; i < end; i++) {
            if(!((start + i ) <  0)) {
                if((start + i )<=  this.nL) {
                    nb += buys.get(start + i);
                    
                } else {
                    return nb;
                    
                }
                
            }
            
        }
        return nb;
    } 
    
    public int getNrSells() {
        int ns = 0;
        int start = bidPosn()+1;
        for(int i=0 ; i < params.getL(); i++) {
            if((start + i) <=  this.nL) {
                ns += sells.get(start + i);    
            } else {
                return ns;
            }
            
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

    public int getAskPosn() {
        return askPosn;
    }

    public void setAskPosn(int askPosn) {
        this.askPosn = askPosn;
    }

    public int getBidPosn() {
        return bidPosn;
    }

    public void setBidPosn(int bidPosn) {
        this.bidPosn = bidPosn;
    }
    
    
    
}
