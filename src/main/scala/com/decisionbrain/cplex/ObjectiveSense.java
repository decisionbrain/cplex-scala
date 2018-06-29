/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex;

import ilog.concert.IloObjectiveSense;

/**
 * Created by dgodard on 10/02/2017.
 */
public enum ObjectiveSense {
    Minimize(-1),
    Maximize(1),
    Unknown(0)
    ;

    private int value;

    ObjectiveSense(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static ObjectiveSense fromString(String value) {
        for (ObjectiveSense sense : values()) {
            if (sense.toString().equalsIgnoreCase(value)) {
                return sense;
            }
        }
        return ObjectiveSense.Unknown;
    }

    public static ObjectiveSense fromInt(int value) {
        for (ObjectiveSense sense : values()) {
            if (sense.getValue() == value) {
                return sense;
            }
        }
        return ObjectiveSense.Unknown;
    }

    public IloObjectiveSense toIloObjectiveSense() {
        IloObjectiveSense sense = null;
        switch (this) {
            case Minimize:
                sense = IloObjectiveSense.Minimize;
                break;
            case Maximize:
                sense = IloObjectiveSense.Maximize;
                break;
        }
        return sense;
    }

}
