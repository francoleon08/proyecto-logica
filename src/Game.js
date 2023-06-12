import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult } from './util';
import { numberToColor } from './util';

let pengine;
let waiting = false;

function Game() {  
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [value, setValue] = useState(0);
  const [path, setPath] = useState([]);
  const [display_game_over, setDisplayGameOver] = useState('none');  
  const [display_movement, setDisplayMovement] = useState('none');  
  const timeOnPath = 300;
  const timeBooster = 200;
  
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
    else {
      waiting = true;
      const gridS = JSON.stringify(grid);
      const pathS = JSON.stringify(path);
      const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";        
      pengine.query(queryS, (success, response) => {
        if (success) {
          setScore(score + joinResult(path, grid, numOfColumns));
          setPath([]);
          animateEffect(response['RGrids'], timeOnPath);
          setValue(0);
          setTimeout(() => {            
            const lastGrid = response['RGrids'][response['RGrids'].length - 1];
            chekGameOver(lastGrid);
          }, (timeOnPath * response['RGrids'].length + 1500));
        }
      });  
    }
  }

  /**
   * Show the intermediate result of a path
   */
  function setPathIntermediate(newPath) {
    if (!checkGrid()) {
      return;
    }
    if(newPath.length > 1) {
      const path = JSON.stringify(newPath); 
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

  /** *
    Called when Booster Collapser is executed
   */
  function onClickBooster() {       
    if (!checkGrid() && waiting) {
      return;
    }
    else {
      waiting = true;
      const gridS = JSON.stringify(grid);
      const queryS = "booster_colapser(" + gridS + "," + numOfColumns + ", RGrids)";
      pengine.query(queryS, (success, response) => {        
        if (success) {   
          setPath([]);
          animateEffect(response['RGrids'], timeBooster);      
          setTimeout(() => {            
            const lastGrid = response['RGrids'][response['RGrids'].length - 1];
            chekGameOver(lastGrid);
          }, (timeBooster * response['RGrids'].length + 1500));
        }
      });     
    }        
  }

  /**
    Called when "Mejor Movimiento" is executed
   */
  function onClickBestMove() {
    if (!checkGrid() && waiting) {
      return;
    }
    else {
      const gridS = JSON.stringify(grid);
      const queryS = "best_move(" + gridS + "," + numOfColumns + ", Path, Result)";           
      pengine.query(queryS, (success, response) => {        
      if (success) {                          
        setPath(response['Path']);
        setValue(response['Result']);
      }      
      });    
    }
  }

  /**
   * Called when "Mejor Movimiento Adyacente" is executed
  */
  function onClickBestMoveAdyacent() {
    if (!checkGrid() && waiting) {
      return;
    }
    else {
      const gridS = JSON.stringify(grid);
      const queryS = "best_move_adyacent(" + gridS + "," + numOfColumns + ", Path, Result)";           
      pengine.query(queryS, (success, response) => {        
      if (success) {
        setPath(response['Path']);
        setValue(response['Result']);
        if(response['Result'] === 0) {
          setDisplayMovement('block');
        }
      }      
      else {
      }
      });    
    }
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
      waiting = false;
    }
  }

  /**
   * Called after the execution of those functions that modify the grid
   */
  function chekGameOver(Grid) {       
    const gridS = JSON.stringify(Grid);
    const queryS = "booster_colapser(" + gridS + "," + numOfColumns + ", _)";                           
    pengine.query(queryS, (success) => {                      
      if (!success) {     
        waiting = true;        
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

  /** 
   * Hide notification of adjacent paths
  */
  function hideAlertMovement() {
    setDisplayMovement('none');
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
          style={waiting ? { backgroundColor: "#ED2B2A", border: "none"} : undefined}
          title="Colapsar bloques iguales"
        >
          <i className='bx bxl-graphql bx-lg bx-tada'style={waiting ? { color: "white" } : { color: "red" }}></i>
        </div>
        <div 
          className="power-up" 
          onClick={onClickBestMove}
          style={waiting ? { backgroundColor: "#ED2B2A", border: "none"} : undefined}
          title="Mejor movimiento"
        >
          <i className='bx bx-bulb bx-lg bx-tada' style={waiting ? { color: "white" } : { color: "yellow" }}></i>
        </div>
        <div 
          className="power-up" 
          onClick={onClickBestMoveAdyacent}
          style={waiting ? { backgroundColor: "#ED2B2A", border: "none"} : undefined}
          title="Mejor movimiento adyacente al mayor bloque"
        >
          <i className='bx bx-qr bx-lg bx-tada' style={waiting ? { color: "white" } : { color: "blue" }}></i>
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
      <div 
        className="best_move"
        style={{display: display_movement}}
      >
        <div className="background_best_move">
          <span>No existe un camino adyacente a algún bloque</span>
          <span             
            className='best_move_close'             
            onClick={hideAlertMovement}
            >
              Cerrar
          </span>
        </div>
      </div>
    </div>    
  );
}

export default Game;