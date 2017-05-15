# Pollfic: Aplicación de organización de encuestas hecha en erlang

En Pollfic los usuarios pueden:

* Servir como host para encuestas.
* Votar en encuestas (que puden estar hosteadas por el propio usuario o por otros nodos de Pollfic).
* Funcionar como un Balancer Node de PollFic.
* Funcionar como un Discover Node de PollFic.

## Vista física de la app

![Vista fisica](https://raw.github.com/braismcastro/pollfic/master/Doc/Physical_view.png)

## Organización de los archivos de la app

![Estructura archivos](https://raw.github.com/braismcastro/pollfic/master/Doc/Project_files.png)

## Configuración de la aplicación

Existe un fichero de configuración  de la aplicación llamado [**config**](https://github.com/braismcastro/pollfic/blob/master/config) que se encuentra en la raíz del repositorio. Su contenido es el siguiente:
```
{discover_dir, {127,0,0,1}}.
{discover_port, 9090}.
{balancer_dir, {127,0,0,1}}.
{balancer_port,8080}.
```
Configuración necesaria para un nodo cliente:
* balancer_dir
* balancer_port

Configuración necesaria para un nodo discover/balancer:
* balancer_dir
* balancer_port
* discover_dir
* discover_port

## Configuración de claves

Antes de poder ejecutar la aplicación sin problemas es necesaria la ejecución del fichero [**pollfic_keys.sh**](https://github.com/braismcastro/pollfic/blob/master/pollfic_keys.sh). Este fichero generará en el directorio raíz un directorio "keys", con dos ficheros de claves pública y privada (public_key.pem y private_key.pem). Este proceso es necesario para el correcto funcionamiento de la aplicación.

Solo es necesario ejecutar el script una vez.

## Instrucciones para compilar

En la raíz del repositorio se encuentra el script [**build.sh**](https://github.com/braismcastro/pollfic/blob/master/build.sh):

```bash

rm -r `dirname $0`/bin
mkdir `dirname $0`/bin
mkdir `dirname $0`/bin/polls
mkdir `dirname $0`/bin/oldpolls 
erlc -o `dirname $0`/bin/ `dirname $0`/src/server.erl `dirname $0`/src/discover.erl `dirname $0`/src/util.erl `dirname $0`/src/dicc.erl `dirname $0`/src/gui.erl `dirname $0`/src/client.erl `dirname $0`/src/filter.erl `dirname $0`/src/encrypt.erl `dirname $0`/src/balancer.erl `dirname $0`/src/mylist.erl
```
* Dar permisos de ejecución al script con ```chmod +x ~/build.sh ```.
* Para compilar, ejecutar el script con ```bash ~/build.sh```.

	 El script creará la estructura de directorios _/bin_ necesaria para la ejecución del programa y guardará el código ejecutable dentro de /bin.
    

* Antes de ejecutar el programa asegurarse de estar dentro del directorio /bin.
* Si para una nueva tarea se tuviese que crear un nuevo fichero, sería necesario modificar el script. Añadir el fichero a la línea de compilación (la de "erlc"), de la misma forma que están referenciados el resto de ficheros. 

## Ejemplo de ejecución
### Arrancar el balancer
        balancer:start().
### Arrancar el servidor discover 
        discover:start().
### Utilizar el cliente
* A través de la interfaz gráfica (Puede dar problemas en linux)
```erlang
        gui:start().
````
* A través de la terminal interactiva de erlang:
```erlang
        client:find_polls().
        client:poll_details({ServerIP, ServerPort, PollName}).
        client:vote({ServerIP, ServerPort, PollName}, positive).
        client:new_poll(PollName, Description).
```
