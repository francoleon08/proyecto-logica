import React from 'react';
import { numberToColor } from './util';

function Square({ value, onClick, onMouseEnter, className }) {
    function valueToString() {        
        let digitos = Math.ceil(Math.log10(value + 1));
        if(value > 16000 && value < 1000000) {
            let digitosRequeridos = Math.floor(digitos / 2);
            let numeroString = value.toString();
            return numeroString.substring(0, digitosRequeridos) + "K";
        }
        else if (value > 1000000){
            let digitosRequeridos = Math.floor(digitos / 4);
            let numeroString = value.toString();
            return numeroString.substring(0, digitosRequeridos) + "M";
        }
        return value.toString();
    }

    // value === 0 means the square is empty.    
    return (
        <div
            className={"square" + (className ? " " + className : "")}
            style={value === 0 ? undefined : { backgroundColor: numberToColor(value) }}
            onClick={onClick}
            onMouseEnter={onMouseEnter}
        >
            {value === 0 ? "" : valueToString()}
        </div>
    );
}

export default Square;