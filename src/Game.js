import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult } from './util';
import { numberToColor } from './util';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [value, setValue] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    setPathIntermediate(newPath);
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,let time = 700;
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";    
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids'], 600);
        setValue(0);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Show the intermediate result of a path
   */
  function setPathIntermediate(newPath) {
    if(newPath.length > 1) {
      let path = "[";
      for (let index = 0; index < newPath.length; index++) {
        
        path += "["+newPath[index]+"]";
        if(index < newPath.length-1)
          path += ",";
      }
      path += "]";
      
      const gridS = JSON.stringify(grid);
      const queryS = "get_result_path(" + gridS + "," + numOfColumns + "," + path + ", Result)";    
      pengine.query(queryS, (success, response) => {
        if (success) {
          const res = response['Result'];
          setValue(res);
        } else {
          setWaiting(false);
        }
      });   
    }
    else {
      setValue(0);
    }
  }

  /*
    Called when Booster Collapser is executed
   */
    function onClickBooster() {      
      const gridS = JSON.stringify(grid);
      const queryS = "booster_colapser(" + gridS + "," + numOfColumns + ", RGrids)";    
      pengine.query(queryS, (success, response) => {        
        if (success) {          
          animateEffect(response['RGrids'], 400);    
        } else {
          setWaiting(false);
        }        
      }); 
    }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids, time) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {      
      setTimeout(() => {
        animateEffect(restRGrids, time);        
      }, time);
    } else {
      setWaiting(false);
    }
  }


  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div className='scoreTitle'>Puntaje: </div>
        <div className="score">{score}</div>
        <div 
          className="value" 
          style={value === 0 ? undefined : { backgroundColor: numberToColor(value) }}
        >
          {value}
        </div>
        <div 
          className="booster" 
          onClick={onClickBooster}
        >Booster Colapser
        </div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
    </div>
  );
}

export default Game;