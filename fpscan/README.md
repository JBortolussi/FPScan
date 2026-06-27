# What is Tiny ?

TINY stands for Tiny Is Not Yasa (Yet Another Static Analyzer).

Tiny is a simple abstract interpretation based static analyzer to be used
for abstract interpretation tutorials (typically by making the students
write simple non relational abstract domains). It was primarily intended
to remain small and simple (less than 2klocs of Ocaml).

# User guide

You can either install tiny as an opam package or in a docker container. 
We advised you to install it with OPAM because it makes using tiny easier.

## OPAM installation

### Dependencies

You need to have OPAM the OCaml Package Manager installed. 
You will find instruction on how to install opam on your system at https://opam.ocaml.org/doc/Install.html

You also need to install `rsync`.

Other packages will be required. The list of the required system package is system-dependent. But don't worry OPAM is able to determine which packages to install.

For information only (no need to install them manually) on a fresh debian-based system the following packages are required : ̀`libgmp-dev libmpfr-dev m4 perl`

### Install

Clone this project with git and open a terminal in the root of the project then run :

``` 
opam pin -n -y . # Add tiny in opam
opam depext tiny # Install the required system dependencies
opam install tiny # Install Tiny
```

You can then use tiny from the command line. Running `tiny` without any input will show the usage instructions.
``` 
$ tiny
tiny: No input file provided.
Usage: tiny [options] <input_filename>
  --abstract-domain <domain>  Use abstract domain <domain> (dummyInt (default), kildall_int, kildall_real, parity, signes_real, signes_int, intervals_infint, intervals_rat, intervals_double, signes_int_parity_red_1, intervals_double_err, affine, polka, oct)
...
``` 

### Uninstall

Just run `opam unpin tiny`

## Docker installation

### Dependencies

The only dependency here is docker.

### Install

In a shell with the root of this project as current directory run :
```sudo docker build --tag tiny .``` 
 in order to build the docker image

For convenience we will define the `tiny-docker` alias.
```alias tiny-docker='sudo docker run --rm -v $(pwd):/workdir -w /workdir --entrypoint "/tiny/bin/tiny" tiny'```
You can add the line above to your .bashrc

### Use


```tiny-docker -a signes_int examples/int/ex01.tiny```

NB: the docker container only has access to your current directory so path to the file to analyze should be relative and under the current directory.

### Uninstall

Just run : `sudo docker image rm tiny`

# Developer guide

## Abreviation

```
asn : assignement
asrt : assertion
nop : no - op (dummy statement)
stm : statement
env : environment
expr : expression
u- (prefix) : (usually) untyped
``` 

## Build 

This project uses [dune](https://dune.readthedocs.io/en/stable/quick-start.html) build system.

The `bin/` folder contains two files :
 * mail.ml which is the entry point of the tiny executable
 * test_domain.ml which defines an executable to test domains
The `/lib` folder defines a private library containing internal tiny modules.

### Build and run executable 

```
dune exec ./bin/main.exe
dune exec ./bin/test_domain.exe
```

### Generating the documentation
```
dune build @doc-private
```
The documentation is generated to ̀_build/default/_doc/_html/tiny@XXXXXXXX/Tiny/index.html`

### Something strange ?

Clean old builds with : `dune clean`

### Managing depencies 

Depencies are managed with dune if you need to add or change a depency just edit the `dune-project` config file at the root of the repository.
The depencies are listed as s-expressions such as :
```
(package 
 (depends
  (opam_package_name1 (>= minimal_version))
  (opam_package_name1 (>= minimal_version))
 )
)
```

## Installing Emacs Mode

If you intend to use Emacs for editing TINY programs, you are advised
to install the provided Emacs mode.

* Add the following at the beginning of you ~/.emacs file:
```
(setq load-path (cons "/directory/in/which/you/put/the/file/tiny.el" load-path))
(autoload 'tiny-mode "tiny" "Major mode for editing TINY code" t)
;; comment this if you don't want to automatically load TINY mode with each TINY file 
(setq auto-mode-alist
      (append
       '(("\\.tiny" . tiny-mode))
       auto-mode-alist))
```

Of course, you'll have to adapt the path to the directory (and not the file)
in which you put the file tiny.el 


# Extensions

## Fix-point arithmetics and machine types

The input language has been extended to specify fixpoint computation.

There are basically two ways to implement it, either manually
definition the shifts or using a higher level syntax, precising the
fixpoint formats.

In both cases, in order to avoid introducing cast operators in the
syntax, all assignements with these two options have to be performed
using three addresses instructions, ie. no nesting of operators.

As an example, if one want to cast an int16_t into a int32_t, we need
to declare a variable with the proper format and make an assignement.

int16_t x;
int32_t y;

x = 34;
y = x; // implicit cast from int16_t to int32_t

Let us now detail the two options, to rely on these machine type and
perform fixpoint arithmetic computation.

### machine integers - the manual approach

In the first version, types should be defined with machine datatypes:

int16_t, uint8_t, ...

The program is parsed but not modified.

It is the responsability of the programmer to perform the proper
shifts. As an example, a division in fixpoint arithmetics shall be
coded as

z = ((x << f) +  / y


fix(


TODO explain:

- uniquement du code 3 adresses
- les variables doivent etre toutes declarees avec leur type
- l'outil fait des choix (questionnables) si les formats ne sont pas compatibles
  - add/sub entre inputs non alignees
  - resultat de l'operation sur un format different du format de la variable resultat 
	- ici certainement un shift et un cast
  - on peut eviter cela en utilisant les casts et les shifts des entiers
  - TODO en cas de ce type d'incoherence, l'outil doit afficher explicitement les conversions et shifts introduits
- le code parse' est donc traduit en code sur des int machines
- on peut ensuite analyser le code avec des domaines d'entier (machine)
- et on peut 
