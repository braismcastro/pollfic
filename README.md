# Pollfic: Aplicación de organización de encuestas hecha en erlang

En Pollfic los usuarios pueden:

* Servir como host para encuestas
* Votar en encuestas (que puden estar hosteadas por el propio usuario o por otros nodos de Pollfic).
* Funcionar como un Discover Node de PollFic

## Vista física de la app

![Vista fisica](https://raw.github.com/braismcastro/pollfic/master/Doc/Pollfic%20architecture_%20physical%20view.png)

## Vista de desarrollo

![Vista de desarrollo](https://raw.github.com/braismcastro/pollfic/master/Doc/pollfic_project_files_structure.png)

## Configuración de la aplicación

Existe un fichero de configuración  de la aplicación llamado [**config**](https://github.com/braismcastro/pollfic/blob/master/config) que se encuentra en la raíz del repositorio. Su contenido es el siguiente:
```
{discover_dir, {127,0,0,1}}.	<- Dirección IP del nodo Discover que usará la aplicación cliente.
{discover_port, 9090}.			<- Puerto en el que escucha el nodo Discover que usará la aplicación cliente.
```
Tratad de no subir cambios sobre este fichero.

## Instrucciones para compilar

En la raíz del repositorio se encuentra el script [**build.sh**](https://github.com/braismcastro/pollfic/blob/master/build.sh):

```bash
rm -r `dirname $0`/bin
mkdir `dirname $0`/bin
mkdir `dirname $0`/bin/polls
mkdir `dirname $0`/bin/oldpolls
erlc -o `dirname $0`/bin/ `dirname $0`/src/server.erl `dirname $0`/src/discover.erl `dirname $0`/src/util.erl `dirname $0`/src/dicc.erl `dirname $0`/src/client.erl
```
* Dar permisos de ejecución al script con ```chmod +x ~/build.sh ```.
* Para compilar, ejecutar el script con ```bash ~/build.sh```.

	 El script creará la estructura de directorios _/bin_ necesaria para la ejecución del programa y guardará el código ejecutable dentro de /bin.
    

* Antes de ejecutar el programa asegurarse de estar dentro del directorio /bin.
* Si para una nueva tarea se tuviese que crear un nuevo fichero, sería necesario modificar el script. Añadir el fichero a la línea de compilación (la de "erlc"), de la misma forma que están referenciados el resto de ficheros. 

### Compilación desde sublime text

* Ir a ```Tools -> Build System -> New Build System... ```
* Escribir el siguiente script y guardarlo con el nombre que gustes:

```
{
	"shell_cmd" : "~/pollfic/build.sh"
}
```

* Cuando sea necesario compilar el código bastará con ```Ctrl + B``` desde sublime.
* Para editar el script lo mejor es ir a ```Preferences -> Browse Packages... ``` y entrar en la carpeta "User". Ahí podrás encontrar el script.

## Work in progress
Tarea | Equipo | Progreso
:------------ | :-------------: | :---:
Funcionalidad básica | Todos los miembros | 100%
Autenticación y cifrado | David y Brais | 0%
Replicación y calanceo de carga | Paloma y Manu | 0%
Filtro del balanceador | Alex y Dani | 0%
