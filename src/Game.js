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
  const [display_game_over, setDisplayGameOver] = useState('none');  
  const [waiting, setWaiting] = useState(false);
  /* setInterval(chekGameOver, 3000); */

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
    if (!checkGrid() && waiting) {
      return;
    }
    setWaiting(true);
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";        
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids'], 300);
        setValue(0);
      }
    });  
  }

  /** 
   * Create Path for pengines
  */
  function createPath(newPath) {
    let path = "[";
      for (let index = 0; index < newPath.length; index++) {
        
        path += "["+newPath[index]+"]";
        if(index < newPath.length-1)
          path += ",";
      }
      path += "]";
      return path;
  }

  /**
   * Show the intermediate result of a path
   */
  function setPathIntermediate(newPath) {
    if (!checkGrid()) {
      return;
    }
    if(newPath.length > 1) {
      let path = createPath(newPath);      
      const gridS = JSON.stringify(grid);
      const queryS = "get_result_path(" + gridS + "," + numOfColumns + "," + path + ", Result)";    
      pengine.query(queryS, (success, response) => {
        if (success) {
          const res = response['Result'];
          setValue(res);
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
    if (!checkGrid() && waiting) {
      return;
    }        
    setWaiting(true);
    const gridS = JSON.stringify(grid);
    const queryS = "booster_colapser(" + gridS + "," + numOfColumns + ", RGrids)";
    pengine.query(queryS, (success, response) => {        
      if (success) {   
        onPathChange([]);
        animateEffect(response['RGrids'], 200);
      }
    });     
  }

  function onClickBestMove() {
    if (!checkGrid()) {
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "best_move(" + gridS + "," + numOfColumns + ", Path, Result)";               
    pengine.query(queryS, (success, response) => {        
    if (success) {                  
      setPath(response['Path']);
      setValue(response['Result']);
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
      setTimeout( () => {
        animateEffect(restRGrids, time);        
      }, time);      
    }
    else {
      setWaiting(false);
    }
  }

  /**
   * Called every 3 seconds, check game over 
   */
  function chekGameOver() {   
    if (!checkGrid() && waiting) {
      return;
    }     
    const gridS = JSON.stringify(grid);
    const queryS = "booster_colapser(" + gridS + "," + numOfColumns + ", RGrids)";               
    pengine.query(queryS, (success, response) => {        
    if (!success) {            
      setDisplayGameOver('block');      
    }      
    });    
  }

  /** 
   * Called to reload the page
  */
  function refreshPage() {
    window.location = window.location.href;
  }

  /**
   * Verify that the grid is valid
   * @returns true that the grid is valid
   */
  function checkGrid() {
    const gridS = JSON.stringify(grid);    
    let state = true;
    if(gridS.includes("[0") || gridS.includes(",0,") || gridS.includes("0]"))
      state = false;    
    return state;
  }


  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div 
          className="score"
          style={value === 0 ? { display: "block" } : { display: "none" }}
        >
          <div>
            <i className='bx bxs-star bx-md bx-tada'></i>
            <span>{score}</span>
          </div>
        </div>       
        <div 
          className="value" 
          style={value === 0 ? undefined : { backgroundColor: numberToColor(value), display: "block" }}
        >
          <div>
            {value}
          </div>
        </div>   
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />      
      <div className="footer">
        <div 
          className="power-up" 
          onClick={onClickBooster}
        >
          Booster Colapser
        </div>
        <div 
          className="power-up" 
          onClick={onClickBestMove}
        >
          Mejor Movimiento
        </div>
      </div>
      <div 
        className="game_over"
        style={{display: display_game_over}}
      >
        <div className="background_game_over">
          <span>Game Over - Puntaje: {score}</span>
          <span className="refresh" onClick={refreshPage}>Click para recargar</span>
        </div>
      </div>
    </div>    
  );
}

export default Game;