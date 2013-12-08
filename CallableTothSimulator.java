package com.mfe.baruch.capstone;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import org.apache.commons.math3.distribution.PoissonDistribution;
import org.apache.commons.math3.distribution.UniformIntegerDistribution;

@SuppressWarnings("rawtypes")
public class CallableTothSimulator implements Callable<RunState> {
    private Parameters runPars;

    public CallableTothSimulator(Parameters pars) {
        this.runPars = pars;

    }

    @Override
    public RunState call() throws Exception {

        return runSimulation();
    }
    
    public RunState runSimulation() {
        Book book = new Book(runPars);

        RunState state = new RunState(book, this.runPars);


            for (int j = 0; j < runPars.getNumEvents(); j++) {
                //System.out.println("Time step: " + j);
                generateTothEvent(state);
                state.getMidPrxs().add(state.getBook().mid());
                
            }

            
            return state;
            
    }

    private void generateTothEvent(RunState state) {
        
            limitOrder(state, getLimitOrders(state));
            marketOrder(state);
            cancelOrder(state);
            
    }

    private double getFraction(RunState state) {
        double rv = state.getFracUnif().sample();
        double frac = 1 - Math.exp(Math.log(rv / runPars.getZetaG()) / (runPars.getZetaG() - 1));
        if(frac > 1.0) {
            System.out.println("Fraction cannot be greater than 1. STOP!!");
        }
        return frac;
        
    }

    private int[] getLimitOrders(RunState state) {
        
        int[] samples = state.getLimPois().sample(2*state.getRunParams().getL());
        return samples;
    }

    private int getCancellations(RunState state) {
        int nrCxlOrds = 0;
        //int[] samples = state.getLimPois().sample(state.getBook().getNrBuys() + state.getBook().getNrSells());
        double lambda = state.getRunParams().getNu()*(state.getBook().getNrBuys() + state.getBook().getNrSells());
        if(lambda <= 0) {
            System.out.println("STOP!!");
        }
        nrCxlOrds = (new PoissonDistribution(lambda)).sample();
//        for (int i = 0; i < samples.length; i++) {
//            nrCxlOrds += samples[i];
//        }
        return nrCxlOrds;

    }

    private void limitOrder(RunState state, int[] nrOrds) {
        int startBuy = state.getBook().askPosn();
        int startSell = state.getBook().bidPosn();
        int m = nrOrds.length / 2;

        for (int i = 0; i < m; i++) {
            if (nrOrds[i] > 0) {
                if((startBuy - i) >= 0) {
                    state.getBook().getBuys().set(startBuy - i , state.getBook().getBuys().get(startBuy - i ) + 1);    
                }
                
            }
          
            if (nrOrds[m + i ] > 0) {
                if((startSell + i) <= state.getBook().getnL()) {
                    state.getBook().getSells()
                    .set(startSell + i, state.getBook().getSells().get(startSell + i) + 1);
                    
                }
            }

        }

    }

    private void cancelOrder(RunState state) {
        int nrCxls = getCancellations(state);
        
        for(int i=0; i<nrCxls; i++) {
            
            if (state.getCxlBS().sample() == 0) {
                cancelBuyOrder(state);
                
            } else {
                cancelSellOrder(state);
                
            }
        }
    }

    private void cancelBuyOrder(RunState state) {
        UniformIntegerDistribution uid = new UniformIntegerDistribution(1, state.getBook().getNrBuys());
        int q = uid.sample();
        int b = state.getBook().askPosn();
        int size = 0;
        for(int i=0; i<state.getRunParams().getL(); i++) {
            size += state.getBook().getBuys().get(b - i);
            if(size >= q) {
                state.getBook().getBuys().set(b-i,state.getBook().getBuys().get(b - i) - 1);
                //System.out.println("Cancel Buy to: " + state.getBook().getBuys().get(b-i));
                return;
            }
        }
        
    }

    private void cancelSellOrder(RunState state) {
        UniformIntegerDistribution uid = new UniformIntegerDistribution(1, state.getBook().getNrSells());
        int q = uid.sample();
        int b = state.getBook().bidPosn();
        int size = 0;
        for(int i=0; i<state.getRunParams().getL(); i++) {
            size += state.getBook().getSells().get(b + i);
            if(size >= q) {
                state.getBook().getSells().set(b+i,state.getBook().getSells().get(b + i) - 1);
                //System.out.println("Cancel Sell to: " + state.getBook().getSells().get(b+i));
                return;
            }
        }

    }

    private void marketOrder(RunState state) {
        if(state.getCurrL() <= 0) {
            double rv = state.getMktbsUnif().sample();
            state.setCurrL((int)Math.round( Math.exp( -Math.log(rv)/(state.getRunParams().getAlpha()))) );
            state.setCurrBS(state.getMktBS().sample());
        }
        
        if(state.getCurrBS() ==0) {
            marketBuyOrder(state);
            
        } else {
            marketSellOrder(state);

        }
        state.setCurrL(state.getCurrL()-1);
    }

    private void marketBuyOrder(RunState state) {
        int askPosn = state.getBook().askPosn();
        state.getBook().getSells().set(askPosn, (int)Math.round(state.getBook().getSells().get(askPosn)*(1 - getFraction(state))));
    }

    private void marketSellOrder(RunState state) {
        int bidPosn = state.getBook().bidPosn();
        state.getBook().getBuys().set(bidPosn, (int)Math.round(state.getBook().getBuys().get(bidPosn)*(1 - getFraction(state))));
    }



}
