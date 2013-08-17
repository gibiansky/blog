Image morphing is the process of interpolating between two images in order to
create something which looks like a nice blend between the two input images. If
you've never played with an image morphing program, go find one - it's
certainly an amusing way to spend a few minutes, especially if you have some
pictures of family and friends lying around to play with.

Here's a fun example of image morphing. The creator took a bunch of pictures of women in art and here is the result of nicely morphing them into each other.

<iframe width="420" height="315" src="http://www.youtube.com/embed/nUDIoN-_Hxs" frameborder="0" allowfullscreen></iframe>

Sometimes, you'll see more amusing pictures on the web, such as&nbsp;<a href="http://www.blorge.com/images/Funwithmorphing_7082/morph6.jpg">this one</a>.

How?
----

In this post, I'll attempt to explain exactly how these image morphs are done, and provide you with some source code to play with if you'd like. In this post, I'll refer to the first picture as the initial image, the second picture as the target image, and the amount of interpolation as $\alpha$.

The process of image morphing has several steps:

<ol>
<li>Subdivide the initial and target image into triangles</li>
<li>Create a mapping between the triangles. One triangle in the initial image must correspond to one triangle in the final image.</li>
<li>Individually morph each triangle</li>
<li>Combine all the triangles into one image</li>
</ol>

Subdivision
-----------

The exact way you subdivide the image into triangles doesn't really matter. What matters is that the triangles are mapped one to one nicely - if a certain triangle on the first image encloses the subject's nose, the corresponding triangle in the target image should <i>also</i>&nbsp;enclose the subject's nose.&nbsp;

Here's an example of a bad mapping:

<br/>
<img border="0" height="200" src="http://4.bp.blogspot.com/-j6yHyNoN-3A/TiBc0RG6-pI/AAAAAAAAAAo/NJoWq-NFuDw/s320/one.png" width="320" />
<center>Default triangle mapping</center>
<br/>

Here's a better one, although not much (these two images don't really map well to each other):

<br/>
<img border="0" height="205" src="http://1.bp.blogspot.com/-rbol3jdhaU4/TiBdoQV_B1I/AAAAAAAAAAs/sUJi1UT4Vn4/s320/two.png" width="320" />
<br/>

Some morphing algorithms even attempt to use&nbsp;<a href="https://ccrma.stanford.edu/%7Ejacobliu/368Report/index.html">computer vision and feature detection</a>&nbsp;to perform automated motion, without having a human adjust the triangle mesh.

Morphing (Affine Transform)
---

In order to morph between the two triangles, we can use coordinate geometry and linear algebra. We are given six points:

- Points A, B, C (from the first triangle)
- Corresponding points a, b, c (from the second triangle)

Since both are just triangles, we know that applying a linear transformation will yield the conversion:

$$a = TA + S$$

$$b = TB + S$$

$$b = TB + S$$

Where $T$ is a transformation matrix (shear, scale, rotate)
$$\left( \begin{array}{ccc} x & y \\ z & w \end{array} \right)$$
and $S$ is the translation matrix
$$\left( \begin{array}{ccc} m \\ n \end{array} \right).$$
Together, these two matrices define a linear transformation.

Since of the equations above dealt with points on a two-dimensional plane, they
were actually two equations each. Coincidentally, our linear transformation is
defined by six variables - the same number of equations we are given. In other
words, the linear transformation we want to use is entirely defined by the two
triangles we've picked. You can go ahead and use WolframAlpha (or a piece of
paper and pen) to solve the system of six equations, resulting in the affine
transform $T(x)$. Also, calculate the inverse $T^{-1}(x)$ - we'll need that
later.

Interpolation
-------------

So, we have our initial and target triangles, and a method of finding the transforms (and inverse transforms) between any two triangles. We have our interpolation factor $\alpha$, describing how far along the interpolation we've gone (1.0 is target image, 0.0 is initial, 0.5 is halfway). What do we do now?

First, pick a triangle in the initial image - we're only morphing one triangle at a time. Now, get the corresponding triangle in the target image, and draw lines connecting each of the point, as shown below. The intermediate triangle - one you want to be calculating, is going to have vertices along those lines.

<br/>
<img border="0" height="166" src="http://4.bp.blogspot.com/-NRn4hgeHxzk/TiBkzITbEkI/AAAAAAAAAA0/FrDFtBAIy9Y/s320/Untitleddrawing+%25281%2529.png" width="320" />
<br/>

For each pixel inside the intermediate triangle, we want to find the color to paint it. Follow these steps:
<ol>
<li>Find the transformation from the intermediate triangle to the initial triangle</li>
<li>Get the color of that pixel, multiplied by $\alpha$</li>
<li>Find the transformation from the intermediate triangle to the target triangle</li>
<li>Get the color of that pixel, multiplied by $1 - \alpha$</li>
<li>Add the two colors, and paint your pixel with the result!</li>
</ol>
Once you apply that algorithm for all pixels inside all triangles, you'll get the appropriate morph.

For your convenience and possible amusement, I've implemented this and put the
result&nbsp;<a
href="https://github.com/gibiansky/experiments/tree/master/morph">on
GitHub</a>.

The result is this video:

<iframe width="420" height="315" src="http://www.youtube.com/embed/QgFX5halk6s" frameborder="0" allowfullscreen></iframe>
