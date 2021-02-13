let canvas = document.getElementById("canvas");
let context = canvas.getContext("2d");

function clear() {
  context.clearRect(0, 0, canvas.width, canvas.height);
}

class V {
  constructor(x, y, z) {
    this.x = parseFloat(x);
    this.y = parseFloat(y);
    this.z = parseFloat(z);
  }
  s2() {
    return `[x: ${this.x}, y: ${this.y}]`;
  }
}

class Triangle {
  constructor(v1, v2, v3, color = '#000') {
    this.t = [v1, v2, v3];
    this.v1 = v1;
    this.v2 = v2;
    this.v3 = v3;
    this.color = color;
  }
  draw() {
    context.beginPath();
    context.fillStyle = this.color;
    context.moveTo(this.v1.x, this.v1.y);
    context.lineTo(this.v2.x, this.v2.y);
    context.lineTo(this.v3.x, this.v3.y);
    context.closePath();
    context.fill();
  }
}

function load_object(obj) {
  const lines = obj.split('\n').filter(x => x.trim().length > 0);
  const vectors = lines.filter(x => x[0] == 'v').map(x => new V(
    ...x.split(' ').slice(1))
  )
  const mesh = lines
    .filter(x => x[0] == 'f')
    .map(x => x.split(' ').slice(1).map(z => vectors[+z - 1]))
    .map(t => new Triangle(...t))
  return mesh;
}

const vector_add = (v1, v2) => {
  return new V(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z);
}

const vector_substract = (v1, v2) => {
  return new V(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z);
}

const vector_normalize = (v) => {
  const l = Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  return new V(v.x / l, v.y / l, v.z / l);
}

const vector_dot_product = (v1, v2) => {
  return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
}

function vector_multiply_by_matrix(i, m) {
  const o = new V(
    i.x * m[0][0] + i.y * m[1][0] + i.z * m[2][0] + m[3][0],
    i.x * m[0][1] + i.y * m[1][1] + i.z * m[2][1] + m[3][1],
    i.x * m[0][2] + i.y * m[1][2] + i.z * m[2][2] + m[3][2],
  );
  const w = i.x * m[0][3] + i.y * m[1][3] + i.z * m[2][3] + m[3][3];
  if (w != 0) {
    o.x /= w;
    o.y /= w;
    o.z /= w;
  }
  return o;
}

const vector_scale_to_canvas = (v) => {
  return new V(
    Math.round((v.x + 1.0) * 0.5 * width),
    Math.round((v.y + 1.0) * 0.5 * height),
    v.z,
  );
}

const triangle_offset = (t, o) => {
  return new Triangle(
    vector_add(t.v1, o),
    vector_add(t.v2, o),
    vector_add(t.v3, o),
  );
}

const triangle_normal = (t) => {
  const line1 = new V(
    t.v2.x - t.v1.x,
    t.v2.y - t.v1.y,
    t.v2.z - t.v1.z,
  );
  const line2 = new V(
    t.v3.x - t.v1.x,
    t.v3.y - t.v1.y,
    t.v3.z - t.v1.z,
  );
  return new V(
    line1.y * line2.z - line1.z * line2.y,
    line1.z * line2.x - line1.x * line2.z,
    line1.x * line2.y - line1.y * line2.x,
  );
}

const triangle_multiply_by_matrix = (t, m) => {
  return new Triangle(
    vector_multiply_by_matrix(t.v1, m),
    vector_multiply_by_matrix(t.v2, m),
    vector_multiply_by_matrix(t.v3, m),
  );
}

const triangle_scale_to_canvas = (t) => {
  return new Triangle(
    vector_scale_to_canvas(t.v1),
    vector_scale_to_canvas(t.v2),
    vector_scale_to_canvas(t.v3),
  )
}

const luminum_to_color = (luminum) => {
  const color_code = Math.round(Math.abs(luminum) * 100 + 100);
  const color_hex = color_code.toString(16).padStart(2, '0');
  return `#${color_hex}${color_hex}${color_hex}`;
}

class mesh {
  constructor() {
    this.m = [];
  }
  draw() {
    for (let t of this.m) {
      t.draw();
    }
  }
  draw_projected(mat_proj, mat_rot_z, mat_rot_x) {
    const triangles = [];
    for (let triangle of this.m) {
      const t_rotated_z = triangle_multiply_by_matrix(triangle, mat_rot_z);
      const t_rotated_x = triangle_multiply_by_matrix(t_rotated_z, mat_rot_x);

      const distance = new V(0.0, 0.0, 8.0);
      const t_translated = triangle_offset(t_rotated_x, distance);

      const normal = vector_normalize(triangle_normal(t_translated));
      const camera_dot_product = vector_dot_product(
        normal,
        vector_substract(t_translated.v1, camera),
      );

      if (camera_dot_product < 0) {
        const light_direction = vector_normalize(new V(0.0, 0.0, -1.0));

        const light_dot_product = vector_dot_product(normal, light_direction);

        const color = luminum_to_color(light_dot_product);

        const t_projected = triangle_multiply_by_matrix(t_translated, mat_proj);

        const t_scaled = triangle_scale_to_canvas(t_projected);
        t_scaled.color = color;

        triangles.push(t_scaled);
      }
    }

    triangles.sort((t1, t2) => {
      const z1 = (t1.v1.z + t1.v2.z + t1.v3.z) / 3.0;
      const z2 = (t2.v1.z + t2.v2.z + t2.v3.z) / 3.0;
      return z2 - z1;
    });

    for (const triangle of triangles) {
      triangle.draw();
    }
  }
}

const ship = new mesh();
ship.m = load_object(ship_obj);

const teapot = new mesh();
teapot.m = load_object(teapot_obj);

const cube = new mesh();
cube.m = [
  // SOUTH
  new Triangle(new V(0.0, 0.0, 0.0), new V(0.0, 1.0, 0.0), new V(1.0, 1.0, 0.0)),
  new Triangle(new V(0.0, 0.0, 0.0), new V(1.0, 1.0, 0.0), new V(1.0, 0.0, 0.0)),
  // EAST                                                      
  new Triangle(new V(1.0, 0.0, 0.0), new V(1.0, 1.0, 0.0), new V(1.0, 1.0, 1.0)),
  new Triangle(new V(1.0, 0.0, 0.0), new V(1.0, 1.0, 1.0), new V(1.0, 0.0, 1.0)),
  // NORTH                                                     
  new Triangle(new V(1.0, 0.0, 1.0), new V(1.0, 1.0, 1.0), new V(0.0, 1.0, 1.0)),
  new Triangle(new V(1.0, 0.0, 1.0), new V(0.0, 1.0, 1.0), new V(0.0, 0.0, 1.0)),
  // WEST                                                      
  new Triangle(new V(0.0, 0.0, 1.0), new V(0.0, 1.0, 1.0), new V(0.0, 1.0, 0.0)),
  new Triangle(new V(0.0, 0.0, 1.0), new V(0.0, 1.0, 0.0), new V(0.0, 0.0, 0.0)),
  // TOP                                                       
  new Triangle(new V(0.0, 1.0, 0.0), new V(0.0, 1.0, 1.0), new V(1.0, 1.0, 1.0)),
  new Triangle(new V(0.0, 1.0, 0.0), new V(1.0, 1.0, 1.0), new V(1.0, 1.0, 0.0)),
  // BOTTOM                                                    
  new Triangle(new V(1.0, 0.0, 1.0), new V(0.0, 0.0, 1.0), new V(0.0, 0.0, 0.0)),
  new Triangle(new V(1.0, 0.0, 1.0), new V(0.0, 0.0, 0.0), new V(1.0, 0.0, 0.0)),
];

const width = 512.0;
const height = 480.0;
const f_aspect_ratio = height / width;
const f_near = 0.1;
const f_far = 1000.0;
const f_fov = 90.0;
const f_fov_rad = 1.0 / Math.tan(f_fov * 0.5 / 180.0 * Math.PI);

const camera = new V(0, 0, 0);

let f_elapsed_time = 0;
setInterval(loop, 100);

const arg = (name) => {
  return parseFloat(document.getElementById(name).innerHTML);
}

function loop() {
  const f_far = 1000.0;
  const f_fov = arg('f_fov'); // 90.0;
  const f_fov_rad = 1.0 / Math.tan(f_fov * 0.5 / 180.0 * Math.PI);

  const mat_proj = [
    [f_aspect_ratio * f_fov_rad, 0, 0, 0],
    [0, f_fov_rad, 0, 0],
    [0, 0, f_far / (f_far - f_near), 1.0],
    [0, 0, (-f_far * f_near) / (f_far - f_near), 0.0],
  ];

  const f_theta = 0.1 * f_elapsed_time;

  const rot_z_speed = arg('rot_z_speed'); // 0.5;
  const mat_rot_z = [
    [Math.cos(f_theta), Math.sin(f_theta * rot_z_speed), 0, 0],
    [-Math.sin(f_theta), Math.cos(f_theta * rot_z_speed), 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1],
  ];

  const rot_x_speed = arg('rot_x_speed'); // 0.5;
  const mat_rot_x = [
    [1, 0, 0, 0],
    [0, Math.cos(f_theta * rot_x_speed), Math.sin(f_theta * rot_x_speed), 0],
    [0, -Math.sin(f_theta * rot_x_speed), Math.cos(f_theta * rot_x_speed), 0],
    [0, 0, 0, 1],
  ];

  clear();
  ship.draw_projected(mat_proj, mat_rot_z, mat_rot_x);

  f_elapsed_time += 1;
}
