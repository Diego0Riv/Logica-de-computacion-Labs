function crearTabla(){
    const regex = /\("([^"]+)","([^"]+)"\)/g;
    const result = [];
    let match;
    let str = document.getElementById('inputText').value;
    console.log(str);
    console.log(typeof str);

    while ((match = regex.exec(str)) !== null) {
        result.push([match[1], match[2]]);
    }
    console.log(result)
    const container = document.getElementById("tabla-container");
    container.appendChild(crearTablaDesdeDatos(result));
}

function convertirTextoATuplasArray(texto) {

    const jsonCompatible = texto
        .replace(/\(\s*"/g, '["') 
        .replace(/"\s*\)/g, '"]') 
        .replace(/"\s*,\s*"/g, '","'); 
    return JSON.parse(jsonCompatible);
}
var cant = 0;
function crearTablaDesdeDatos(datos) {
    cant++;
    const filas = 8;
    const columnas = 8;

    const div =document.createElement("div");
    const h2 = document.createElement("h2");
    h2.innerHTML = "Binairo " + cant;
    div.appendChild(h2);
    const tabla = document.createElement("table");

    for (let i = 1; i <= filas; i++) {
      const fila = document.createElement("tr");

      for (let j = 1; j <= columnas; j++) {
        const celda = document.createElement("td");
        celda.textContent = ""; 


        const match = datos.find(([clave, valor]) => {
        if (valor !== "true") return false;
        
        const tipo = clave[0]; // Primer carácter: "s" o "l"
        if (tipo !== "s" && tipo !== "l") return false; // solo aceptamos "s" o "l"
        
        const coords = clave.slice(1).split('_');
        const fila = parseInt(coords[0]);
        const columna = parseInt(coords[1]);

        return fila === i && columna === j;
        });

        if (match) {
          const tipo = match[0][0]; 
          celda.textContent = tipo;
          celda.classList.add(tipo);
        }

        fila.appendChild(celda);
      }

      tabla.appendChild(fila);
    }
    div.appendChild(tabla);
    return div;
  }

